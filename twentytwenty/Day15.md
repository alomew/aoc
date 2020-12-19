# Day 15

This was a difficult day for Haskell because the best way I could see to solve this problem is a lookup table that is repeatedly overwitten --- what we want to lookup is the last turn on which a certain number was seen.

To also complete part 2 before the sun flickers out, the optimization I think we were looking for is a *compact* structure that acts like a lookup table. I will now show that a fixed size array (here I used a fixed size mutable vector) is perfectly justified.

### The sequence has an easy bound over finitely many terms

To use an array we want to know ahead of time how large to make it. But the induced value at place `n` (call it `f(n)`) in the sequence is at most `n - 2`, since if `f(n - 1) = f(t)` (`t` maximal with this property), then `f(n) = n - 1 - t`, or it is `0` (when `f(n-1)` is original). So if we are looking for the `N`th element of the sequence we can construct an array that is the largest of `N` and the elements in the starting sequence, and this array will be sufficiently large.

So provided we are happy storing 30 000 000 numbers in a single array, we can proceed, with no concerns of memory flying all over the place trying to deal with a hashmap or similar.