#![allow(dead_code)]

mod v0 {
    // {{{

    //! Recursion is often the most natural tool for solving problems, for example one of the ways to
    //! calculate factorial is using recursion: factorial of 0 is 1, factorial of any other number
    //! is this number multiplied by factorial of a previous

    // recursion
    fn fac(v: usize) -> usize {
        if v == 0 {
            1
        } else {
            v * fac(v - 1)
        }
    }

    #[test]
    fn fac_works() {
        assert_eq!(fac(4), 24);
    }
} // }}}

mod v1 {
    // {{{
    //! Often recursion is the most natural way of dealing with recursive structure.
    //! Consider a linked list
    #[derive(Debug, Clone)]
    enum List {
        Cons(usize, Box<List>),
        Nil,
    }

    fn make_sample(end: usize) -> List {
        if end == 0 {
            List::Nil
        } else {
            List::Cons(end, Box::new(make_sample(end - 1)))
        }
    }

    impl List {
        fn sum(&self) -> usize {
            match self {
                List::Cons(v, i) => v + i.sum(),
                List::Nil => 0,
            }
        }

        fn prod(&self) -> usize {
            match self {
                // by the power of copy-paste first version had .sum() here :)
                List::Cons(v, i) => v * i.prod(),
                List::Nil => 1,
            }
        }
    }

    #[test]
    fn make_works() {
        let list = make_sample(10usize);

        assert_eq!(list.sum(), 45);
        assert_eq!(list.prod(), 362880);
    }

    /*
    #[test]
    fn also_fails() {
        let list = make_sample(100000usize);
        assert_eq!(list.sum(), 45);
    }
    */
} // }}}

mod v2 {
    // {{{
    //! Let's take out the recursion from the code that user needs to write :)

    //! As before unfolding operation takes a seed value and produces a list out

    /// From unfolding point of view List is one of:
    /// - A pair of a value and the rest of the list that was created from the leftover value
    /// - Nil - end of the list
    #[derive(Debug, Clone)]
    enum List {
        Cons(usize, Box<List>),
        Nil,
    }

    /// ListF is basically the same datatype, except in the second case instead
    /// of having user to generate the whole tree - we let them specify just the leftover seed
    #[derive(Debug, Clone)]
    enum ListF<R> {
        Cons(usize, R),
        Nil,
    }

    // if you squint really hard those types are identical, or can be identical - Rust typesystem
    // won't let you do it: all we need is to set value inside R to be Box with this type itself
    // inside. Basically ListF<Box<ListF<Box<ListF<Box<ListF<.... with turtles all the way down.
    //
    // In Haskell you can say something like this:
    //
    // type Fix<F> = F<Box<Fix<F>>>
    //
    // which defines a fixed point of a Functor
    //
    // And type `Fix<ListF>` corresponds to this infinitely long datatype. Sadly higher rank types
    // are not a thing in Rust...

    /// The unfolder should take the seed value - and a function to produce
    /// the next value. We'll call this function - coalgebra, and the whole unfolder is going to be
    /// called `ana`, short for "anamorphism". Names come from math and come with a lot of
    /// interesting theory :)
    fn ana<S, F>(seed: S, mut coalgebra: F) -> List
    where
        F: FnMut(S) -> ListF<S>,
    {
        match coalgebra(seed) {
            ListF::Cons(x, seed) => {
                let nested = ana(seed, coalgebra);
                List::Cons(x, Box::new(nested))
            }
            ListF::Nil => List::Nil,
        }
    }

    /// And function for unrolling list becomes this:
    fn unroll(seed: usize) -> ListF<usize> {
        if seed == 0 {
            ListF::Nil
        } else {
            ListF::Cons(seed, seed - 1)
        }
    }

    /// Folder function takes recursive list and an algebra and called `cata`, short for "catamorphism"
    fn cata<R, F: FnMut(ListF<R>) -> R>(list: List, algebra: &mut F) -> R {
        match list {
            List::Cons(v, nested) => {
                let nested = cata(*nested, algebra);
                algebra(ListF::Cons(v, nested))
            }
            List::Nil => algebra(ListF::Nil),
        }
    }

    /// Two simple folding algebras: one that calculates sum
    fn sum(base: ListF<usize>) -> usize {
        match base {
            ListF::Cons(a, b) => a + b,
            ListF::Nil => 0,
        }
    }

    /// And a product for all the values in a list
    fn prod(base: ListF<usize>) -> usize {
        match base {
            ListF::Cons(a, b) => a * b,
            ListF::Nil => 1,
        }
    }

    #[test]
    fn cata_ana_works() {
        let l = ana(9, unroll);
        assert_eq!(cata(l, &mut sum), 45);

        let l = ana(9, unroll);
        assert_eq!(cata(l, &mut prod), 362880);
    }

    /// Then there's hylo which is short for hylomorphism. It takes a seed, coalgebra for unfolding
    /// and algebra for folding and produces the folded result.
    /// It can be trivially implemented with `cata` and `ana`, but what's more interesting -
    /// type signature doesn't mention `List` anywhere, so technically it is possible to implement
    /// algorithms on recursive datatypes without creating those recursive datatypes first, yay.
    fn hylo<S, A, R, C>(seed: S, coalg: A, alg: &mut C) -> R
    where
        A: FnMut(S) -> ListF<S>,
        C: FnMut(ListF<R>) -> R,
    {
        cata(ana(seed, coalg), alg)
    }

    #[test]
    fn hylo_works() {
        assert_eq!(hylo(9, unroll, &mut sum), 45);
        assert_eq!(hylo(9, unroll, &mut prod), 362880);
    }
} // }}}

mod v3 {
    // {{{
    //! Now that we separated recursive algorithm from recursive structure - let's try to swap out
    //! the recursive structure to something else

    /// Starting from the same ListF we get the data in/out of the structure
    /// instead of making a linked list we can store it in a vector where value R
    /// contains the index of the next item.
    #[derive(Debug, Clone)]
    enum ListF<R> {
        Cons(usize, R),
        Nil,
    }

    /// If you squint hard enough - this gives more or less the same representation as a linked
    /// list, but with less allocations at expense of making it harder to modify

    struct Ix(usize);
    struct List {
        items: Vec<ListF<Ix>>,
    }

    /// anamorphism puts items in a vector
    fn ana<S, F>(mut seed: S, mut coalgebra: F) -> List
    where
        F: FnMut(S) -> ListF<S>,
    {
        let mut items = Vec::new();
        loop {
            match coalgebra(seed) {
                ListF::Cons(v, s) => {
                    items.push(ListF::Cons(v, Ix(items.len() + 1)));
                    seed = s;
                }
                ListF::Nil => {
                    items.push(ListF::Nil);
                    break;
                }
            }
        }
        List { items }
    }

    /// Catamorphism takes items from a vector. There's several ways to implement it, this method
    /// folds the whole structure from the end keeping the intermediate results in a vector.
    /// For linked list only a single element of the scratch vector will ever have a value but
    /// for different structures we'll look later at it might be not the case.
    fn cata<R, F>(list: List, algebra: &mut F) -> R
    where
        F: FnMut(ListF<R>) -> R,
    {
        let items = list.items;
        let mut scratch = items.iter().map(|_| Option::<R>::None).collect::<Vec<_>>();

        for (ix, item) in items.into_iter().enumerate().rev() {
            scratch[ix] = Some(match item {
                ListF::Cons(v, Ix(d)) => algebra(ListF::Cons(v, scratch[d].take().unwrap())),
                ListF::Nil => algebra(ListF::Nil),
            })
        }
        scratch[0].take().unwrap()
    }

    /// hylomorphism - same idea - cata + ana and no intermediate linked list in between :)
    fn hylo<S, A, R, C>(seed: S, ana_alg: A, cata_alg: &mut C) -> R
    where
        A: FnMut(S) -> ListF<S>,
        C: FnMut(ListF<R>) -> R,
    {
        cata(ana(seed, ana_alg), cata_alg)
    }

    /// And absolutely no changes from user's point of view:
    fn unroll(seed: usize) -> ListF<usize> {
        if seed == 0 {
            ListF::Nil
        } else {
            ListF::Cons(seed, seed - 1)
        }
    }

    fn sum(base: ListF<usize>) -> usize {
        match base {
            ListF::Cons(a, b) => a + b,
            ListF::Nil => 0,
        }
    }

    fn prod(base: ListF<usize>) -> usize {
        match base {
            ListF::Cons(a, b) => a * b,
            ListF::Nil => 1,
        }
    }

    #[test]
    fn hylo_works() {
        assert_eq!(hylo(9, unroll, &mut sum), 45);
        assert_eq!(hylo(9, unroll, &mut prod), 362880);
    }
} // }}}

mod v4 {
    // {{{

    //! And finally - generalization. `cata` and `ana` are tightly coupled to the struct it
    //! operates on, and it really doesn't have to:

    use std::collections::VecDeque;
    // snippet from the anamorphism function earlier
    // match coalgebra(seed) {
    //     ListF::Cons(x, seed) => {
    //         let nested = ana(seed, coalgebra);
    //         List::Cons(x, Box::new(nested))
    //     }
    //     ListF::Nil => List::Nil,
    // }
    //
    // All it does is taking a value from the coalgebra user gave us and replaces seed
    // value with something else, or when folding - replaces a subtree with the folding result
    //
    //
    //  match list {
    //      List::Cons(v, nested) => {
    //          let nested = cata(*nested, algebra);
    //          algebra(ListF::Cons(v, nested))
    //      }
    //      List::Nil => algebra(ListF::Nil),
    //  }

    // this is can be generalied with a Functor :)

    trait Functor<A> {
        type Target<B>; // Target<B> should really be Self<B>, but no higher order types in Rust
        fn fmap<B>(self, f: impl FnMut(A) -> B) -> Self::Target<B>;
    }

    struct Ix(usize);

    trait Rec: Sized {
        fn ana<S, A>(seed: S, alg: A) -> Vec<<Self as Functor<S>>::Target<usize>>
        where
            A: Fn(S) -> Self,
            Self: Functor<S>,
        {
            let mut flat = Vec::new();
            let mut queue = VecDeque::<S>::new();
            queue.push_back(seed);
            while let Some(item) = queue.pop_front() {
                let r = alg(item).fmap(|s| {
                    queue.push_back(s);
                    flat.len() + 1
                });
                flat.push(r);
            }
            flat
        }

        fn cata<R, A>(items: Vec<<Self as Functor<usize>>::Target<R>>, alg: A) -> R
        where
            A: Fn(<<Self as Functor<usize>>::Target<R> as Functor<usize>>::Target<R>) -> R,
            Self: Functor<usize>,
            <Self as Functor<usize>>::Target<R>: Functor<usize>,
        {
            let mut scratch = items.iter().map(|_| Option::<R>::None).collect::<Vec<_>>();
            for (ix, item) in items.into_iter().enumerate().rev() {
                let pop = item.fmap(|i| scratch[i].take().unwrap());
                scratch[ix] = Some(alg(pop));
            }
            scratch[0].take().unwrap()
        }
    }

    #[derive(Debug)]
    enum ListF<R> {
        Cons(usize, R),
        Nil,
    }
    impl<A> Functor<A> for ListF<A> {
        type Target<B> = ListF<B>;

        fn fmap<B>(self, mut f: impl FnMut(A) -> B) -> Self::Target<B> {
            match self {
                ListF::Cons(i, a) => ListF::Cons(i, f(a)),
                ListF::Nil => ListF::Nil,
            }
        }
    }

    fn unroll(seed: usize) -> ListF<usize> {
        if seed == 0 {
            ListF::Nil
        } else {
            ListF::Cons(seed, seed - 1)
        }
    }

    // no changes :)
    fn sum(base: ListF<usize>) -> usize {
        match base {
            ListF::Cons(a, b) => a + b,
            ListF::Nil => 0,
        }
    }

    fn prod(base: ListF<usize>) -> usize {
        match base {
            ListF::Cons(a, b) => a * b,
            ListF::Nil => 1,
        }
    }

    impl<R> Rec for ListF<R> {}

    #[test]
    fn still_works() {
        let s = ListF::ana(9, unroll);
        assert_eq!(45, ListF::cata(s, sum));

        let l = ListF::ana(9, unroll);
        assert_eq!(362880, ListF::cata(l, prod));
    }
} // }}}

// lots more possibilities :)
