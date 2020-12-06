# Day 1

Not much to say about today, as is often the case with first days.

The given solution is not particularly succinct, but it does slightly more than the explicit requirements, namely:

* The operations is generalized for any `n` elements summing to any number
* Finds all such groups, not just the first

## UPDATE: 6th Dec:

I have left the generalised function in, but have written succinct but equivalent
solutions to the two parts.

Note that `x:xs <- tails ps` is used to enumerate the choices "first element will be x; others will be in xs".