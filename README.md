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
