mod v1 {
    #[derive(Debug, Clone)]
    enum List {
        Cons(usize, Box<List>),
        Nil,
    }

    fn make_sample(end: usize) -> List {
        let mut l = List::Nil;
        for a in (1..end).rev() {
            l = List::Cons(a, Box::new(l));
        }

        l
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
}

mod v2 {
    #[derive(Debug, Clone)]
    enum List {
        Cons(usize, Box<List>),
        Nil,
    }

    #[derive(Debug, Clone)]
    enum ListF<R> {
        Cons(usize, R),
        Nil,
    }

    // anamorphyms
    fn ana<S, F: FnMut(S) -> ListF<S>>(seed: S, mut algebra: F) -> List {
        match algebra(seed) {
            ListF::Cons(x, seed) => {
                let nested = ana(seed, algebra);
                List::Cons(x, Box::new(nested))
            }
            ListF::Nil => List::Nil,
        }
    }

    // catamorphism
    fn cata<R, F: FnMut(ListF<R>) -> R>(list: List, algebra: &mut F) -> R {
        match list {
            List::Cons(v, nested) => {
                let nested = cata(*nested, algebra);
                algebra(ListF::Cons(v, nested))
            }
            List::Nil => algebra(ListF::Nil),
        }
    }

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
    fn cata_ana_works() {
        let l = ana(9, unroll);
        assert_eq!(cata(l, &mut sum), 45);

        let l = ana(9, unroll);
        assert_eq!(cata(l, &mut prod), 362880);
    }

    // hylomorphism
    fn hylo<S, A: FnMut(S) -> ListF<S>, R, C: FnMut(ListF<R>) -> R>(
        seed: S,
        ana_alg: A,
        cata_alg: &mut C,
    ) -> R {
        cata(ana(seed, ana_alg), cata_alg)
    }

    #[test]
    fn hylo_works() {
        assert_eq!(hylo(9, unroll, &mut sum), 45);
        assert_eq!(hylo(9, unroll, &mut prod), 362880);
    }
}

mod v3 {

    struct Ix(usize);
    struct List {
        items: Vec<ListF<Ix>>,
    }

    #[derive(Debug, Clone)]
    enum ListF<R> {
        Cons(usize, R),
        Nil,
    }

    // anamorphism
    fn ana<S, F: FnMut(S) -> ListF<S>>(mut seed: S, mut algebra: F) -> List {
        let mut items = Vec::new();
        loop {
            match algebra(seed) {
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

    fn cata<R, F: FnMut(ListF<R>) -> R>(list: List, algebra: &mut F) -> R {
        // Pay no attention to that man behind the curtain :)
        //
        // This code works with ListF shape specifically and works with any R.
        // to be shape agnostic I'd use temporary allocated vector with results full of None
        let mut items = list.items;
        items.pop();
        let mut cur = algebra(ListF::Nil);
        while let Some(item) = items.pop() {
            match item {
                ListF::Cons(x, _) => cur = algebra(ListF::Cons(x, cur)),
                ListF::Nil => unreachable!(),
            }
        }
        cur
    }

    fn cata2<R, F: FnMut(ListF<R>) -> R>(list: List, algebra: &mut F) -> R {
        // this one is uglier but should work for any structure
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

    // hylomorphism
    fn hylo<S, A: FnMut(S) -> ListF<S>, R, C: FnMut(ListF<R>) -> R>(
        seed: S,
        ana_alg: A,
        cata_alg: &mut C,
    ) -> R {
        cata(ana(seed, ana_alg), cata_alg)
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

    #[test]
    fn hylo_works() {
        assert_eq!(hylo(9, unroll, &mut sum), 45);
        assert_eq!(hylo(9, unroll, &mut prod), 362880);
    }
}

mod v4 {
    use std::collections::VecDeque;

    struct Ix(usize);

    trait Functor<A> {
        type Target<B>;
        fn fmap<B>(self, f: impl FnMut(A) -> B) -> Self::Target<B>;
    }

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

    #[derive(Debug, Clone)]
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
    fn asdf() {
        let s = ListF::ana(9, unroll);
        assert_eq!(45, ListF::cata(s, sum));

        let l = ListF::ana(9, unroll);
        assert_eq!(362880, ListF::cata(l, prod));
    }
}
