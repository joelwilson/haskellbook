# How does (fmap . fmap) typecheck?

Write it out, the book said.

```
(.)  :: (b -> c) -> (a -> b) -> a -> c
--       fmap         fmap
fmap :: Functor f => (m -> n) -> f m -> f n
fmap :: Functor g => (x -> y) -> g x -> g y
```

Let's do this. (.) type breakdown is:

`(b -> c)                   -> (a -> b)                   -> a        -> c`

Substitute the two fmaps:
`((m -> n) -> (f m -> f n)) -> ((x -> y) -> (g x -> g y)  -> (x -> y) -> (f m -> f n)`

So this means `m` is `g x` and `n` is `g y`! So let's substitute those and remove the first two args (since they are applied now.
`(x -> y) -> f (g x) -> f (g y)`

GHCI says:
`(fmap . fmap) :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)`

So it looks like it's right! Crazy!


My question now is, how do people discover that functions like this can
be composed together if it is not at all obvious from looking at the
type signatures? Practice makes perfect?
