---
title: Recursion schemes for Rust
author: Michael Baykov
---

# Recursive functions

- **Sepulka** – pl.: sepulkas, a prominent element of the civilization of Ardrites from the
  planet of Enteropia; see *Sepulkaria*
- **Sepulkaria** – sing: sepulkarium, establishments used for sepuling; see *Sepuling*
- **Sepuling** – an activity of Ardrites from the planet of Enteropia; see *Sepulka*


<!-- column_layout: [1, 1] -->
<!-- column: 0 -->
```rust
fn fac(v: usize) -> usize {
    if v <= 1 {
        1
    } else {
        v * fac(v - 1)
    }
}
```
<!-- column: 1 -->
```
fac(4) =>
4 * fac(3) =>
4 * (3 * fac(2)) =>
4 * (3 * (2 * fac(1))) =>
4 * (3 * (2 * 1)) =>
4 * (3 * 2) =>
4 * 6 =>
24
```

<!-- end_slide -->

# Tail call recursion

Recursion doesn't mean using stack, tail recursion can work without allocating values on a
stack. In a sufficiently smart compiler...
<!-- column_layout: [1, 1] -->
<!-- column: 0 -->
```rust
fn fac(v: usize) -> usize {
    fn go(v: usize, acc: usize) {
        if v == 1 {
            acc
        } else {
            go(v - 1, acc * v)
        }
    }
    go(v, 1)
}
```


<!-- column: 1 -->
```
fac(4) =>
go(4, 1) =>
go(3, 4) =>
go(2, 12) =>
go(1, 24) =>
24
```

<!-- end_slide -->
# Recursive data types
Linked list is a typical example of recursive data structure, easiest way to produce it is using recursive function
<!-- column_layout: [1, 1] -->
<!-- column: 0 -->
```rust
enum List {
    Cons(usize, Box<List>),
    Nil,
}

fn unfold(seed: usize) -> List {
    if seed == 0 {
        List::Nil
    } else {
        List::Cons(seed,
            Box::new(unfold(seed - 1))
        )
    }
}
```

<!-- column: 1 -->
```
unfold(3) =>
Cons(3, unfold(2)) =>
Cons(3, Cons(2, unfold(1))) =>
Cons(3, Cons(2, Cons(1, unfold(0)))) =>
Cons(3, Cons(2, Cons(1, Nil))) =>
```

<!-- end_slide -->

# Consuming recursive data types

Same goes about consuming. You need to be careful about calling the right method though

<!-- column_layout: [1, 1] -->
<!-- column: 0 -->
```rust
impl List {
    fn sum(&self) -> usize {
        match self {
            List::Cons(v, i) =>
                v + i.sum(),
            List::Nil => 0,
        }
    }

    fn prod(&self) -> usize {
        match self {
            List::Cons(v, i) =>
                v * i.prod(), // !!!
            List::Nil => 1,
        }
    }
}
```

<!-- column: 1 -->

```
sum(Cons(3, Cons(2, Cons(1, Nil)))) =>
3 + sum(Cons(2, Cons(1, Nil))) =>
3 + 2 + sum(Cons(1, Nil)) =>
3 + 2 + 1 + sum(Nil) =>
3 + 2 + 1 + 0 =>
6
```

```
prod(Cons(3, Cons(2, Cons(1, Nil)))) =>
3 * prod(Cons(2, Cons(1, Nil))) =>
3 * 2 * prod(Cons(1, Nil)) =>
3 * 2 * 1 * prod(Nil) =>
3 * 2 * 1 * 1 =>
6
```
<!-- end_slide -->

# But there are problems

## User needs to be aware of the internals

```rust
    Cons(usize, Box<List>),
//              ^^^
...
    List::Cons(end, Box::new(unfold(end - 1)))
//                  ^^^^^^^^
```

## Stack space is limited
```
unfold(1000000) =>

thread 'main' has overflowed its stack
fatal runtime error: stack overflow
```

<!-- end_slide -->

# Taking the recursion out

<!-- column_layout: [1, 1] -->
<!-- column: 0 -->
This example uses explicit recursion and user needs to deal with recursive calls and internal
representation specific to Rust
```rust
fn unfold(seed: usize) -> List {
    if seed == 0 {
        List::Nil
    } else {
        List::Cons(seed,
            // recursive structure
            // created from the
            // next seed: seed - 1
            Box::new(unfold(seed - 1))
        )
    }
}
```

<!-- column: 1 -->

```
0 => Nil
i => List::Cons(i, i-1)
```

```rust
enum ListF<R> {
    Cons(usize, R),
    Nil,
}

fn unfold(seed: usize) -> ListF<usize> {
    if seed == 0 {
        ListF::Nil
    } else {
        ListF::Cons(seed, seed - 1)
    }
}
```

<!-- end_slide -->
# Infinite data types

Rust types are limited :)

<!-- column_layout: [1, 1] -->
<!-- column: 0 -->
```rust
enum List {
    Cons(usize, Box<List>),
    Nil,
}
```
<!-- column: 1 -->
```rust
enum ListF<R> {
    Cons(usize, R),
    Nil,
}
```

<!-- reset_layout -->

`List` is the same as `ListF<Box<ListF<Box<ListF<Box<ListF<Box....`


<!-- column_layout: [1, 1] -->
<!-- column: 0 -->
```
Option: * -> *
usize: *

Option + usize => Option<usize>
Option<usize>: *
```

<!-- column: 1 -->

```
Fix: (* -> *) -> *
ListF: * -> *

Fix + ListF => Fix<ListF>
Fix<ListF>: *
```

<!-- end_slide -->

# Anamorphism - Geek preposition meaning "upwards"


<!-- column_layout: [2, 1] -->
<!-- column: 0 -->
```rust
fn ana<S, F>(seed: S, mut coalgebra:
        impl FnMut(S) -> ListF<S>) -> List {
    match coalgebra(seed) {
        ListF::Cons(i, seed) => {
            let nested = ana(seed, coalgebra);
            List::Cons(i, Box::new(nested))
        }
        ListF::Nil => List::Nil,
    }
}

fn simple(seed: usize) -> ListF<usize> {
    if seed == 0 {
        ListF::Nil
    } else {
        ListF::Cons(seed, seed - 1)
    }
}
```
<!-- column: 1 -->
Sample unfolding
```
ana(3, simple) =>
Cons(3, Cons(2, ...))
```

And it's already in stdlib :)

```rust
fn successors<T, F>(
    first: Option<T>,
    succ: F) ->
        Successors<T, F>
where
    F: FnMut(&T) ->
        Option<T>
```

`S` ~ `Option<T>`
<!-- end_slide -->

# Catamorphisms - Greek preposition meaning "downwards"

<!-- column_layout: [5, 4] -->
<!-- column: 0 -->
```rust
fn cata<R, F>(list: List, alg: &mut F) -> R
where
    F: FnMut(ListF<R>) -> R, {
    match list {
        List::Cons(v, nested) => {
            let n = cata(*nested, alg);
            alg(ListF::Cons(v, n))
        }
        List::Nil => alg(ListF::Nil),
    }
}
```

<!-- column: 1 -->
```rust
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
```

<!-- end_slide -->

# Hylomorphisms - from the Aristotelian philosophy that form and matter are one

```rust
fn hylo<S, A, R, C>(seed: S, coalg: A, alg: &mut C) -> R
where
    A: FnMut(S) -> ListF<S>,
    C: FnMut(ListF<R>) -> R,
{
    cata(ana(seed, coalg), alg)
}
```

```
hylo(9, unroll, &mut sum) => 45
hylo(9, unroll, &mut prod) => 362880
```

**hylo** operates on a recursive data type without having the recursive datatype itself in its type
signature! Maybe we don't need the data type?

<!-- end_slide -->

# Removing the recursion from recursive structure


```rust
enum List {
    Cons(usize, Box<List>), // <- Box is a pointer to recursive structure
                            //    allocated somewhere on a heap, we can't
                            //    have it in ListF
    Nil,
}
```

```rust
enum ListF<R> {
    Cons(usize, R),
    Nil,
}

type List = Vec<ListF<usize>>; // <- usize is a pointer to a sibling item
                               //    allocated somewhere in this Vec
```

<!-- end_slide -->

# Anamorphism for a flattened type

<!-- column_layout: [5, 4] -->
<!-- column: 0 -->
Anamorphism function without recursion makes a linked list without recursion
```rust
fn ana<S, F>(seed: S, mut co: F) -> List
where
    F: FnMut(S) -> ListF<S>,
{
    let mut items = Vec::new();
    let mut seed = seed;
    while let ListF::Cons(v, s) = co(seed) {
        let ix = items.len() + 1;
        items.push(ListF::Cons(v, ix));
        seed = s;
    }
    items.push(ListF::Nil);
    List { items }
}
```

<!-- column: 1 -->

Same unfold function as before!
```rust
fn unfold(s: usize) -> ListF<usize> {
    if seed == 0 {
        ListF::Nil
    } else {
        ListF::Cons(s, s - 1)
    }
}
```
```
ana(unfold, 4) =>

[ Cons(4, 1)
, Cons(3, 2)
, Cons(2, 3)
, Cons(1, 4)
, Nil
]
```

<!-- end_slide -->

# Catamorphisms

```rust
fn cata<R, F>(list: List, algebra: &mut F) -> R
where
    F: FnMut(ListF<R>) -> R,
{
    let items = list.items;
    // scratch buffer holds intermediate results
    let mut scratch = items.iter()
        .map(|_| Option::<R>::None)
        .collect::<Vec<_>>();

    // folding without recursion from right to left
    for (ix, item) in items.into_iter().enumerate().rev() {
        scratch[ix] = Some(algebra(match item {
            ListF::Cons(v, d) =>
                algebra(ListF::Cons(v, scratch[d].take().unwrap())),
            ListF::Nil =>
                algebra(ListF::Nil),
        }))
    }
    scratch[0].take().unwrap()
}
```

<!-- end_slide -->

# Problems with current implementations
- `cata` and `ana` are tightly coupled with `ListF` so to use them you first have to write a
  bucnh of boilerplate code

# Good things about current implementations
- no recursion - no stack overflow!
- amortized allocations and cache friendly access in `Vec` means it's about 3x faster

# Let's try to decouple `cata`/`ana` from `ListF`

I removed some decorations and we have this code, both lines are talking about applying custom
`F(A) -> B` to `ListF<A>` to get `ListF<B>` - this is a `Functor`!

```rust
// ana
    ListF::Cons(v, seed) => ListF::Cons(v, /* process seed */),
    ListF::Nil => ListF::Nil,

// cata
    ListF::Cons(v, res) => ListF::Cons(v, /* process res */),
    ListF::Nil => ListF::Nil,
```

<!-- end_slide -->

# Functor, recap

```rust
trait Functor<A> {
    type Target<B>; // <- Self<B>, but Rust types are sad
    fn fmap<B>(self, f: impl FnMut(A) -> B) -> Self::Target<B>;
}

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
```

<!-- end_slide -->

# Structure agnostic versions of `ana`

```rust
trait Rec: Sized {
    fn ana<S, A>(seed: S, alg: A)
        -> Vec<<Self as Functor<S>>::Target<usize>>
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
    ...
```

<!-- end_slide -->

# Structure agnostic versions of `cata`

Rant on absense of type constraints :)
```rust
    fn cata<R, A>(items: Vec<<Self as Functor<usize>>::Target<R>>,
        alg: A) -> R
    where
        A: Fn(<<Self as Functor<usize>>::Target<R>
            as Functor<usize>>::Target<R>) -> R,
        Self: Functor<usize>,
        <Self as Functor<usize>>::Target<R>: Functor<usize>,
    {
        let mut scratch = items.iter()
            .map(|_| Option::<R>::None).collect::<Vec<_>>();
        for (ix, item) in items.into_iter().enumerate().rev() {
            let pop = item.fmap(|i| scratch[i].take().unwrap());
            scratch[ix] = Some(alg(pop));
        }
        scratch[0].take().unwrap()
    }
}
```

