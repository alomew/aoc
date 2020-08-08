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

## Day 14

This is quite a nice problem of resource management.

My solution depends on three points of interest: structure,
modular arithmetic, and search.

We must understand the structure of the dependencies formed by
our given list of transmutations. Though it should be said that I make more
assumptions on this structure
in my solution than are outright stated in the problem.

We have an acyclic, directed graph where the nodes are recipes
that point to the recipes of all its ingredients
(unless the only ingredient is `ORE`, in which case there are no outgoing edges).
(Note this structure
does not relate to quantities anywhere).

We now show that it is acyclic:
We take as given that
every substance except `ORE` has a single method of production,
and everything except `ORE` can be made.
So a general cycle would be like:
producing `A` requires having `B`;
producing `B` requires having `C`;
...;
producing `G` requires having `A`.
But then we cannot ever produce `A` -- a contradiction.

Every substance having a single method of production
means that if ever we ask a recipe to be made,
there is a single, determined way to find further dependencies:
follow all departing edges until you reach recipes
taking only `ORE`.

I model the actual question -- of how much ore is needed --
by asking how much of its product each recipe needs to produce.
As is often the case with graphs, it is by a recursive function acting
in the presence of limited mutable state.
That state is where we keep track of how many times
a certain substance has been asked for.
(So, if a recipe produces 7 `F`s and we have stored the number 10 next to it,
we mean that we have required 10 `F`s -- not 70).

The flow is like this. Ask for `q`-many `X`s. Update the register to
say that we need `q`-many `X`s. We now ask how many times the recipe must be
made. For instance the recipe might be for (`q`-1)-many `X`s and will thus
need to be made twice in order to produce *at least* `q`-many `X`s.
We then ask each ingredient in that recipe be produced as many times as needed:
i.e., as many times as required in one recipe multiplied by the
number of recipes required.
This continues.
Whenever we encounter a recipe used before,
we update the register by adding the new quantity required
to the amount stored for that recipe.
We then request ingredients only if we need (a) new batch(es) of the product.
For instance, if we have made two batches of 6 `F`s because we previously needed
7 of them, then asking for 3 more will not trigger a new production,
because we have 5 left over from the previous two batches.
However, asking for *another* 3 on top of that *will* trigger a new batch.

So, in short, we are traversing the graph, prompted by our register.

Once this process finishes, we are left to calculate the amount of `ORE`
required.
We check only the recipes in the register who has `ORE` for an ingredient
(indeed we observe that whenever `ORE` in *an* ingredient
it is *the only* ingredient, even though it is not formally guaranteed).
Then we check how many times we would have to call the recipe
(round up a division) and how many `ORE` each recipe requires.

This latter calculation is quite straightforward, but it is
effectively a nice version of the calculation
you have to make when choosing how many
new batches of some intermediate substance you need.
It is nice because it is final -- it knows the waste it incurs
is required.
You cannot apply this calculation to the intermediary substances
without incurring an unnecessary waste.

We notice that a batch should be produced any time its required-tally
exceeds a multiple of the quantity produced. So if I ask for 1 `F` and
its recipe produces a batch of 6, we make a batch since `1 > 0 * 6`.
If I ask for 24 `G` in batches of 5, we make five batches since
``` text
24 > 0 * 5
24 > 1 * 5
24 > 2 * 5
24 > 3 * 5
24 > 4 * 5
```
Now if we ask for 1 more `G`, we need no more batches, since in five batches
we have made 25 `G`s already.
But if we ask for 2, then we need another, since `26 > 5 * 5`.

So the question we are in fact answering is how many times in a certain range --
if we have already required `p`-many of a substance and now ask for `q` more,
our range is `[p + 1, ..., p + q]`, integers --
we find a number `m` s.t. `m = 1 (mod b)`, where `b` is the batch size.
This is, by the way, equivalent to finding multiples of `b` in the range
`[p, ..., p + q - 1]`. The solution is left to the reader, but my version
can be found in code form as `zero-congs`.

The second part is interesting, and falls (after an additional assumption)
into a nice category of problems --
search problems.
It is trivial to extend part 1 to find the `ORE` required to
make `n` `FUEL`. Not by multiplying, of course, because you are then wasting
leftover intermediary resources.
Recall that to start the chain of requests for 1 `FUEL`, you
have to ask for a certain quantity of it (since the function is also called
when asking for certain quantities of other recipes):
we just expose this choice.

We make a new assumption: the process of producing `FUEL` is expensive in `ORE`.
This needn't hold. Suppose the following list of reactions:
``` text
10 ORE => 1 A
1 A => 4000 C
1 C => 1 FUEL
```
Here, we see that with 10 `ORE` I could produce 4000 `FUEL`.
But to constrain our
search space, we suppose that the amount of `FUEL`
we can produce is less than
`ORE` we consume.

Now, we can binary search over the set of possible `FUEL` yields:
`[0, ..., MAX-ORE]`.

However, we can see without any new assumptions that
the function `ore-needed` -- taking a number of `FUEL` to the number
of `ORE` required to produce that much `FUEL` -- is monotone increasing:
more `FUEL` requires at least as many `ORE`;
less `FUEL` requires at most as many `ORE`.

These are the conditions required for this binary search.

At each stage we either
* exclude numbers below an attainable bound.
  
  (if you have enough `ORE` for 10 `FUEL`,
  ignore 0-9)
* exclude numbers above and including an unattainable bound.
  
  (if you don't have enough `ORE` to make 9 `FUEL`, ignore 0-9)

(Enough `ORE` is assessed by using `extended-part-1` to find out
how much `ORE` would be required to make `n` `FUEL` and asking
whether we have at least as much `ORE` available -- exactly as you
would expect).

This pruning eventually gives the maximum fuel we can produce.
