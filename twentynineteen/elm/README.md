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

## Day 3

This day was an interesting one -- our code is being tested for efficiency: I saw an "a process is holding up this website" message.

The second part was particularly interesting in that it asked a question of you -- do you rewrite everything you have or do you write new functionality around the old stuff? My approach wasn't that great -- I am pretty sure I slowed down both parts with my changes.

Perhaps tomorrow it will be time to return to Dart, and see if I can figure out some of the cool iterator stuff that is likely to come very handy, or the getting around Maybes, which is convoluting half of my code.

## Day 4

A strange day this was -- the naive solution seems perfectly reasonable. And any improvement over it seems far too 
complex to warrant considering. Combinatorics will have to wait.
My initial attempt at part 2 was rather cumbersome; and the benefit of the second attempt, which makes use of the 
almost indispensable `List.Extra` module, is that you could leverage parallel computing.

However, we do start to worry that Elm is not performant enough to deal with more demanding tasks. Yes, the abstraction
is there, but the answer page is taking a couple of seconds to show.