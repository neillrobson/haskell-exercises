# Haskell Exercises

> Neill's experiments and solutions to the exercises in the Haskell Programming from First Principles book.

## Notes

### Identity and Constant

Think of `Constant` like a 2-tuple, but always ignoring the second value.
Just like how, for two-tuple's `Applicative`, the first type needs to be a
`Monoid` (so that they can be squished together),
The first type of `Constant` must also be a `Monoid` for similar squishing.

### Compose

The `Compose` datatype squishes two "structure types" into one.
Say you have a `Maybe [Integer]`. That's two structures (`Maybe` and `List`)
around the Integer data.
A regular traverse or bind operation would just lift the outermost structure:
in this case, the `Maybe`.
If instead you traversed or bound a `Compose Maybe [Integer]`,
the lift would go directly to the `Integer` data.
The abstract type of `Compose` is `Compose g f a`: since classes like `Monad`
and `Traversable` treat all but the last type parameter as untouchable,
`g` and `f` (`Maybe` and `[]`) are both lifted and `a` (`Integer`) is worked on.

### Alternative

Some `Applicative` instances have Monoidal properties in the failure cases.
For the `Left` type in `Either`, each failure needs to be squished together
when they are encountered in a chain of `<*>`.

The `Alternative` typeclass gives the Monoid treatment to the wrapper itself.
The `empty` field is the identity, and `<|>` is the associative operation.
Using `Maybe` as an example, `Nothing` in the first argument will yield the second argument
(so `Nothing` is the value for `empty`). Compare this behavior with passing `Nothing`
as the first argument to `<*>`, which is guaranteed to output `Nothing` regardless
of the second argument.

The functions `some` and `many` **are not relevant to every `Alternative` instance**.
They are only meaningful for types that represent *repeatable actions*, operations that
could fail after executing a certain number of times.

```haskell
some :: f a -> f [a]
some v = (:) <$> v <*> many v

many :: f a -> f [a]
many v = some v <|> pure []
```

`Maybe` doesn't make sense here: if `v` is `Nothing`, the results are trivial,
and if `v` is `Just whatever`, neither evaluation terminates.
The `Maybe` type never changes between success and failure through successive interpretations.

Consider instead a parser type. If `p` is a parser for a single character,
`some p` would parse as many of that character as possible,
failing if there are none. `many p` behaves similarly, but would still succeed
on zero occurrences.
