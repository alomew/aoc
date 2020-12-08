# Day 8

## Part 1

In this part we produce a stream of states that our
program is in when executing these instructions.
As per the problem statement, this is an infinite list
but such things are allowed in Haskell.

We then run through these states looking for when
our pointer returns to a past value, and return the accumulator at that point.

## Part 2

We first generate all the swapped instruction lists,
where `nop` and `jmp` swap.
This is done by mapping each position in the list to the list where the instruction at that point is swapped
(or ignored if it's an `acc`).

Next, we have a recursive function that traverses a list
of states and returns the accumulator if the index is too large (we have terminated); nothing if we detect a repeat (in the same way as part 1); recurses if we can deduce neither at the current state.

Applying this to all the state streams for the respective instruction lists gives what we want.