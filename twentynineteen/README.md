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
