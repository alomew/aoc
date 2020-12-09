# Day 9

My suspicions that many of the problems from days 1-8
would be extended in future problems seem to be
materialising...

# Part 1 

Here we use our `expense2` function from day 1,
slightly modified, and `divvy` from the ever-useful `split` package
to find the entries which are a sum of numbers from the previous however-many elements.

The broad strategy is to consider overlapping blocks of `n + 1` elements and check whether the `last` number in each block is the sum of two different numbers in the `init` (all-but-last) of the block.

## Part 2 

Here we devise a strategy that boils down to this:

* Take your block to be the first element of the list
* At each stage:
    * if the sum of the block is too small, extend it at the end
    * if the sum of the block is too big, drop the first element
* Continue this concertina-ing until you get a block of the right sum