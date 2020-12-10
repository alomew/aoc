# Day 10

## Part 1

The statement for this part is full of
information that is only
necessary for Part 2.

In Part 1 we are only forming a single chain ---
the chain that uses all of the adapters --- and computing
joltage differences between successive adapters, charge points, etc.

To do this we employ `divvy` to break the list of sorted
adapters into pairs, and then map these pairs to their difference. Then we can count differences of 1 and 3 as required.

## Part 2

The insight here is to notice that we can construct a graph
of adapters that can be plugged into each other.
I.e. the graph has the edge condition `i --> j` exactly when `j - i = 1,2,3`. Our problem then boils down to
counting paths from `0` to `high`, where `high` is the joltage on your device.

There is a neat recursive solution here: the number of paths from `i` to `high` is the sum of the number of paths from `j` to `high`, for all the nodes `j` such that `i --> j` .
We can think of this like: 

    At each stage I can only choose an adapter that has joltage +1, +2, or +3 above the current one. And each one will give a different final arrangement. So I can add up the arrangements *after I've made each choice* to get the total number

The base case of this recursion is: `#paths(from high to high) = 1`

However performing the naive recursion leads to plenty of repetition. Say for instance you are currently on `5` jolts and you have the adapters `6`, `7`, and `8`. Then you can continue with `5 --> 7` or with `5 --> 6 --> 7`. But the number of arrangements after both of these will be the same by our recursion argument: the number of paths from `7` to `high` is a fixed number.

So we can create a store which associates `j` with `#paths(from j to high)` --- every time we calculate a path count we put its value in the store. We get our answer by retrieving the value associated to `0`.

The implementation of this store uses Haskell's laziness to calculate values in the store in terms of other values in the store.