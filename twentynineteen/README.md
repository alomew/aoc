# 2019

This year will be done using Elm. I initially thought I'd try out Dart, but I reckon I'll be more likely to go further if I use a language I'm more familiar
with the idioms for.

## File Structure

The output of each day and part is displayed to me by `src/Main.elm`. Each day's functionality is written in `src/Day[i]`.

## Day 1

The first part is a simple map and sum. The second is slightly more involved -- we have to handcook a recursive function, which we make tail-recursive just to be safe.

## Day 2

The difficulties with the first part lay in the slight difficulty in accessing
Elm arrays (we cannot guarantee the length of the array in the type, so `Maybe`s are used.)

The second part, though perhaps a more elegant solution exists, was accomplished here
with a brute force recursion.

Most relevant to the Elm approach is that we can see these two parts
collaborate rather nicely. Every time we run `runProgram` we get a test case for `findNounVerb`.
So, provided we trust the implementation of the former, we can fuzz test the latter
by generating an intcode program, finding the output, and then asking `findNounVerb`
to give us the noun and verb already in the program. (Of course we could purposefully
spoil these places if we mistrusted the implementer.) If I had needed more test cases,
or if it hadn't been 23:54, I might have done this.

EDIT 23DEC19: Actually the generation of an intcode program is non-trivial -- you have to guarantee that positional pointers
actually point to valid positions.

## Day 3

This day was an interesting one -- our code is being tested for efficiency: I saw an "a process is holding up this website" message.

The second part was particularly interesting in that it asked a question of you -- do you rewrite everything you have or do you write new functionality around the old stuff? My approach wasn't that great -- I am pretty sure I slowed down both parts with my changes.

Perhaps tomorrow it will be time to return to Dart, and see if I can figure out some of the cool iterator stuff that is likely to come very handy, or the getting around Maybes, which is convoluting half of my code.

## Day 4

A strange day this was -- the naive solution seems perfectly reasonable. And any improvement over it seems far too
complex to warrant considering. Combinatorics will have to wait.
My initial attempt at part 2 was rather cumbersome; and the benefit of the second attempt, which makes use of the
almost indispensable `List.Extra` module, is that you could read it in French and still understand it.

## Day 5

Focusing again on the intcode computer, we pull out a separate module (you can tell we will be coming back to this), and hodgepodge together an interpreter properly, with variants for different instructions and string parsers to translate the ints in the given intcode.

I also decided to use the Elm REPL, rather than a barebones HTML page, to give me the final answers, because
it is much faster.

## Day 6

This problem concerns a tree (with no obvious cute properties). I rushed to represent the orbital relations with a recursive variant type (as you might a BST) but this actually did not help to solve either part. They are both really quite messy. The worst offence is the parsing of the input -- we receive the orbital pairs in arbitary order, so we have to search the whole tree for every pair as we insert it.

Despite this implementation issue, I enjoyed the set theory employed to finish part 2.

## Day 10

For the first part, we capture the ray on which every asteroid finds itself.
Then the asteroid we are searching for is the one which permits the most
inhabited rays.

For the second part, we call the special asteroid the *source*.
We construct -- relative to the source --
the displacements of all the asteroids, grouped by which ray emanating
from the source they lie on, sorted with first being closest to the source,
last being farthest.
We then cycle around, removing the heads of these lists.
The order of the cycle is clockwise: to determine this order we use the
`atan2` function, which in Racket is an overload on `atan`.
This cycle must also begin with the ray pointing straight up,
and I accomplished this with a transformation on the result of `atan`
such that -π points directly up (and π is excluded): for in that case,
the least angle is the one
we wish to start with.
This method, however, is unlikely to scale, since you would be relying
on ever precise floating point numbers.

## Day 12

The first part of this problem is effectively following the guidance.
It boils down to a time stepping function that acts two-fold:
by adjusting velocities by a simplified gravity,
and then adjusting positions by these updated velocities.
In our notation we will say: `time-step = apply-velocity ∘ apply-gravity`.
Note that composition invokes the right function, and then the left.

For the second part, in order to finish in good time,
and in reasonable space, we can make the following observations:

* The `time-step` function is dimension-independent.
  This is since gravity is defined dimensionwise, and the application of
  velocity is vector addition.
* The `time-step` function is injective (stepping two different systems always
  results in two new, still different systems).

  We can deduce this by noticing `time-step` has an inverse.
  Firstly, `apply-velocity` does not affect velocity, so we can subtract
  velocity from position to undo its effect.
  Now `apply-gravity` does not affect position, so once we have undone
  `apply-velocity` we know the position *before* gravity has been taken
  into account. We can then apply gravity in reverse.
  Then by composing these inverses, we have an inverse for `time-step`.
  Call it `time-back`.
* Suppose we had a sequence of systems indexed by time like `S(i)`.
  System here comprises all positions and velocities of all objects.
  If ever we have a sequence `S(0) - - - S(k) - - - S(n)=S(k)`
  we know that `time-step^k(S(0)) = S(k)` and so also `time-back^k(S(k)) = S(0)`,
  but then since `S(n) = S(k)`, `time-back^k(S(n)) = S(0)`. So we can say
  `S(n-k) = S(0)`. I.e. if we ever revisit a system we have already seen
  then we must have already seen the initial system again. Also, since `time-step`
  is deterministic, we must have a recurring cycle.

The only thing not proved above is that a cycle *must* exist. But the question
assumes it, so I am happy to as well for the time being.

The solution, then, is to find the cycle times in each dimension,
and notice that we will return to the initial state at every multiple
of these times. The lowest common multiple is thus our solution.
