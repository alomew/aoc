# 2019

This year will be done using Elm. I initially thought I'd try out Dart, but I reckon I'll be more likely to go further if I use a language I'm more familiar
with the idioms for.

## File Structure

The output of each day and part is displayed to me by `src/Main.elm`. Each day's functionality is written in `src/Day[i]`.

## Day 1

The first part is a simple map and sum. The second is slightly more involved -- we have to handcook a recursive function, which we make tail-recursive just to be safe.