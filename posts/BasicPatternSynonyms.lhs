---
title: Pattern Synonym Basics
published: April 28, 2022
highlighting: pygments
tags: [haskell, programming]
synopsis: An introduction to pattern synonyms in Haskell
---

This post is a literate Haskell file, so as always, pragmas, a module header, and some imports. It
is up to date as of GHC 9.2.1 and uses GHC2021. Basic Haskell knowledge and an understanding of
view patterns are assumed.

> {-# LANGUAGE DerivingStrategies #-}
> {-# LANGUAGE PatternSynonyms #-}
> {-# LANGUAGE ViewPatterns #-}
> {-# OPTIONS_GHC -Wall -Wno-unused-top-binds #-}
>
> module BasicPatternSynonyms
>  ( SortedList ((:>), Nil, getSortedList)
>  )
> where
>
> import Data.List (insert, sort)

<h2 id="unsnoc">`unsnoc`</h2>

It is fairly trivial to write a function to "uncons" a list from the back end. Whether it is a good
idea to do this in general is not the topic of this post (the answer is that it probably is not).

> unsnoc :: [a] -> Maybe (a, [a])
> unsnoc [] = Nothing
> unsnoc [x] = Just (x, [])
> unsnoc (x : xs) = fmap (x :) <$> unsnoc xs

> -- >>> unsnoc [1..5]
> -- Just (5,[1,2,3,4])

> -- >>> unsnoc []
> -- Nothing

However, it can be unwieldy to use this as control flow. One option is to use pattern guards:

> sum1 :: Num a => [a] -> a
> sum1 [] = 0
> sum1 xs | Just (y, ys) <- unsnoc xs = y + sum1 ys

Another is to use a view pattern:

> sum2 :: Num a => [a] -> a
> sum2 [] = 0
> sum2 (unsnoc -> Just (x, xs)) = x + sum2 xs

However, not only are these not ergonomic, but they also generate `incomplete-patterns` warnings,
since GHC doesn't understand that the second equation of each covers the `_ : _` case of the list
type. If this this is a common occurrence, you may wish for something a bit more ergonomic and type
safe. A pattern synonym fills this role perfectly.

<h2 id="pattern-synonyms">Pattern Synonyms</h2>

Pattern synonyms look much like ordinary function definitions, consisting of a (usually optional)
type signature and an equation. They are enabled by the `PatternSynonyms` GHC extension, which can
be turned on in a `.cabal` file, with the `-XPatternSynonyms` flag in a shell, or with a `LANGUAGE`
pragma at the top of a file, as shown above. A trivial pattern synonym might look like this:

> pattern Affirmative :: Bool
> pattern Affirmative = True

This creates a new *pattern* `Affirmative`. You can use it the same way you'd use the pattern
`True`. For instance:

> toBit1 :: Bool -> Int
> toBit1 Affirmative = 1
> toBit1 _ = 0

We can also write a similar pattern for `False`:

> pattern Negative :: Bool
> pattern Negative = False

Now we can rewrite `toBit1` using both pattern synonyms.

> toBit2 :: Bool -> Int
> toBit2 Affirmative = 1
> toBit2 Negative = 0

But alas, GHC issues an `incomplete-patterns` for `toBit2`. Although we've covered all possible
cases of `Bool`, GHC still doesn't know that this is the case:
```
    Pattern match(es) are non-exhaustive
    In an equation for ‘toBit2’:
        Patterns of type ‘Bool’ not matched:
            False
            True
```
In order to convince GHC that our patterns do cover the input, we can use a `COMPLETE` pragma.

> {-# COMPLETE Affirmative, Negative #-}

This pragma takes a comma-separated list of patterns and instructs GHC that those patterns used in
combination fully cover their input. Be careful: GHC doesn't attempt to verify that this is true.
You can easily suppress a valid `incomplete-patterns` warning with a bad `COMPLETE` pragma.

> pattern Truth :: Bool
> pattern Truth = True
>
> {-# COMPLETE Truth #-}
>
> bad :: Bool -> String
> bad Truth = "true"

GHC generates no error or warning that `bad` does not cover its input, and applying it to `False`
will yield a runtime error.

> -- >>> bad True
> -- "true"
>
> -- >>> bad False
> -- BasicPatternSynonyms.hs: Non-exhaustive patterns in function bad

Patterns are not limited to simple values. For example, a pattern that matches through two layers
of `Maybe`s:

> pattern JustJust :: a -> Maybe (Maybe a)
> pattern JustJust x = Just (Just x)

> pattern JustNothing :: Maybe (Maybe a)
> pattern JustNothing = Just Nothing

> {-# COMPLETE JustJust, JustNothing, Nothing #-}

The first thing that might look odd here is the type of `JustJust`. You may intuitively want the
arrow to go the other direction: `Maybe (Maybe a) -> a`, since the pattern is "extracting" the `a`
from within the nested `Maybe`s. However, pattern synonym signatures are a little bit different
from normal functions. The final section is called the scrutinee type and is placed at the far
right of the signature, after the final `->` if one exists. For both `JustJust` and `JustNothing`,
the scrutinee type is `Maybe (Maybe a)`. This is the input type against which we can attempt to
match these patterns. Preceding the final arrow are the argument types. `JustNothing` has no
arguments, and `JustJust` has a single argument of type `a`. Let's see how we might use these
patterns in a function:

> doubleMaybe :: b -> b -> (a -> b) -> Maybe (Maybe a) -> b
> doubleMaybe _ _ f (JustJust x) = f x
> doubleMaybe _ z _ JustNothing = z
> doubleMaybe z _ _ Nothing = z
>
> -- >>> doubleMaybe 1 2 (+ 1) Nothing
> -- 1

Note that this function does not generate any warnings, since we marked this set of patterns
as covering via the `COMPLETE` pragma above.

<h2 id="directionality">Directionality</h2>

Pattern synonyms defined with an `=` are called "bidirectional" or "implicitly bidirectional." This
means that they can also be used as an *expression* in addition to a pattern.

> -- >>> :type JustNothing
> -- Maybe (Maybe a)
>
> -- >>> doubleMaybe 1 2 (+ 1) JustNothing
> -- 2
>
> -- >>> doubleMaybe 1 2 (+ 1) (JustJust 3)
> -- 4

In this way, they behave a lot like regular data constructors. The other possibilities for pattern
synonyms are "unidirectional" and "explicitly bidirectional." A unidirectional pattern synonym is
defined with a left arrow `<-` instead of the equals sign `=`.

> pattern Yes :: Bool
> pattern Yes <- True

Patterns defined this way cannot be used in an expression.

Explicitly bidirectional pattern synonyms are also defined with a left arrow, but additionally have
a `where` clause which defines the pattern a second time, this time with an `=`. The definition in
the where clause is just like an ordinary function, and is used when the pattern is used as an
expression.

> pattern Yeah :: Bool
> pattern Yeah <- True
>   where
>     Yeah = False

> -- >>> :type Yeah
> -- Bool
>
> -- >>> Yeah
> -- False

The pattern `Yeah` matches the value `True`, but when used as an expression produces the value
`False`. It can be confusing if you define pattern synonyms that behave counterintuitively like
this, but GHC will make no attempt to stop you.

> nah :: Bool -> String
> nah Yeah = "nah"
> nah _ = "yeah"

> -- >>> nah Yeah
> -- "yeah"

This is confusing! The pattern `Yeah` doesn't match the expression `Yeah`. I strongly recommend
taking care to avoid this kind of behavior by defining sensible bidirectional pattern synonyms.
When possible, let GHC do it for you by using an *implicitly* bidirectional pattern synonym.

<h2 id="unsnoc-again">`unsnoc` (again)</h2>

Equipped with this knowledge, we can return to our dilemma with `unsnoc` from above. We can define
an explicitly bidirectional pattern synonym for it with a view pattern:

> pattern (:<) :: [a] -> a -> [a]
> pattern xs :< x <- (unsnoc -> Just (x, xs))
>   where
>     xs :< x = xs <> [x]
>
> {-# COMPLETE (:<), [] #-}

Let's break that down. The signature defines the type of both arguments as well as the scrutinee.
One argument as a list (the "init" of the scrutinee), and another as a single value (the "last" of
the scrutinee). The type of the scrutinee that we're matching against is a list of any type.
```haskell
pattern (:<) :: [a] -- The type of the first argument
             -> a   -- The type of the second argument
             -> [a] -- The type of the scrutinee
```
The next line defines `:<` as used in a pattern. Here we use the `unsnoc` that we defined above as
a view pattern, matching its result against `Just (x, xs)`, which brings `x :: a` and `xs :: [a]`
into scope. We then bind them as the arguments to our new pattern `:<`. It maybe feel a little
backwards, defining variables on the right and then using them on the left, but this is how pattern
synonyms work.
```haskell
pattern xs :< x <- -- (3) use the bound `x` and `xs` as the arguments to the pattern synonym
  (unsnoc ->       -- (1) apply `unsnoc` to the scrutinee as a view pattern
    Just (x, xs))  -- (2) match the result of the application against the pattern `Just (x, xs)`
```
The `where` clause defines how our pattern `:<` behaves when used as an expression. We should
strive to make sure that values created by `:<` will successfully match against `:<`. Here we do
that by appending the second argument to the first.
```haskell
  where
    xs :< x = xs <> [x]
```
The `COMPLETE` pragma indicates that this pattern combined with the pattern `[]` form a complete
set for matching against lists, allowing us to use them in definitions without an
`incomplete-patterns` warning.
```haskell
{-# COMPLETE (:<), [] #-}
```
Now we can rewrite our `sum` functions from before with our new pattern. It is much more ergonomic,
and also doesn't generate any warnings.

> sum3 :: Num a => [a] -> a
> sum3 [] = 0
> sum3 (xs :< x) = x + sum3 xs

> -- >>> sum3 [1..5]
> -- 15

Since we defined it as explicitly bidirectional, we can also use our pattern to construct lists by
appending to the end.

> -- >>> [1..5] :< 6
> -- [1,2,3,4,5,6]

<h2 id="patterns-as-predicates">Patterns as Predicates</h2>

Another use of pattern synonyms is to check properties of data. For instance we might match against
a sorted list like this:

> sorted :: Ord a => [a] -> Bool
> sorted xs = sort xs == xs

> insertSorted1 :: Ord a => a -> [a] -> Maybe [a]
> insertSorted1 x xs | sorted xs = Just (insert x xs)
> insertSorted1 _ _ = Nothing

We can clean this up with a pattern synonym.

> sorted' :: Ord a => [a] -> Maybe [a]
> sorted' xs | sorted xs = Just xs
>            | otherwise = Nothing

> pattern Sorted :: Ord a => [a] -> [a]
> pattern Sorted xs <- (sorted' -> Just xs)
>   where
>     Sorted xs = sort xs

This pattern synonym has a constraint. Specifically, this is known as a *required* constraint,
because it must be satisfied by the context in order to use the pattern. Now we can write
`insertSorted` with a simple pattern match by using `Sorted`:

> insertSorted2 :: Ord a => a -> [a] -> Maybe [a]
> insertSorted2 x (Sorted xs) = Just (insert x xs)
> insertSorted2 _ _ = Nothing

Because we defined it bidirectionally, we can also use the `Sorted` pattern in an expression to
sort lists.

> -- >>> Sorted [3, 2, 6, 5]
> -- [2,3,5,6]

<h2 id="matching-abstract-types">Matching Abstract Types</h2>

Consider the type of sorted lists. We might represent that as an abstract type via a `newtype` like
this:

> newtype SortedList a = UnsafeSortedList { getSortedList :: [a] }
>   deriving stock (Show)

> sortedList :: Ord a => [a] -> Maybe (SortedList a)
> sortedList (Sorted xs) = Just (UnsafeSortedList xs)
> sortedList _ = Nothing

Naturally, we don't export the data constructor `UnsafeSortedList` to avoid people constructing
a `SortedList` which isn't actually sorted. This has the advantage of having no runtime overhead
over ordinary lists, since `newtype` wrappers are erased at compile time. However, matching against
a sorted list becomes a hassle for consumers who don't have access to the constructor, once again
requiring view patterns or pattern guards. For instance, extracting the head of a sorted list
(which is the minimum, of course):

> sortedMin :: SortedList a -> Maybe a
> sortedMin xs | x : _ <- getSortedList xs = Just x
> sortedMin _ = Nothing

We can use pattern synonyms to define a safe list-like interface for this type.

> pattern (:>) :: Ord a => a -> SortedList a -> SortedList a
> pattern x :> xs <- UnsafeSortedList (x : (UnsafeSortedList -> xs))
>   where
>     x :> UnsafeSortedList xs = UnsafeSortedList (insert x xs)
>
> infixr 5 :>

> pattern Nil :: SortedList a
> pattern Nil = UnsafeSortedList []

> {-# COMPLETE (:>), Nil #-}

Now we can construct sorted lists in a manner similar to lists, with it automatically being sorted
as its constructed.

> -- >>> 4 :> 2 :> 5 :> 3 :> Nil
> -- UnsafeSortedList {getSortedList = [2,3,4,5]}

We can also deconstruct sorted lists just like ordinary lists:

> sumSorted :: (Num a, Ord a) => SortedList a -> a
> sumSorted Nil = 0
> sumSorted (x :> xs) = x + sumSorted xs

This leads to some interesting definitions, such as this insertion sort:

> insertionSort :: Ord a => [a] -> SortedList a
> insertionSort = foldr (:>) Nil

> -- >>> insertionSort [7, 6, 2, 4, 7, 4, 5, 9]
> -- UnsafeSortedList {getSortedList = [2,4,4,5,6,7,7,9]}

We can also "bundle" our pattern synonyms together with our type constructor when exporting them.
Instead of a usual export like `SortedList (getSortedList)`, we can include the pattern synonyms we
defined `SortedList ((:>), Nil, getSortedList)`. Then when a consumer imports the type with `import
BasicPatternSynonyms (SortedList (..))`, they get the pattern synonyms as well. This is demonstrated
at the beginning of the post.

<h2 id="resources">Additional Resources</h2>

The official documentation for pattern synonyms lives in
<a href="https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/pattern_synonyms.html">
the GHC user's guide</a>, though I don't think it's very good.

Richard Eisenberg also has a
<a href="https://youtu.be/SPC_R5nwFqo">few</a>
<a href="https://youtu.be/00HxIPG0vW0">excellent</a>
<a href="https://youtu.be/_KOqBvZD6HU">videos</a>
on the topic.
