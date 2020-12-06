# Day 5

This day was cute. The trick is to see that the seat ids can be recovered
by replacing the appropriate letters with 0s and 1s, giving the binary representation
of the id. At first, I only saw this for the two parts separately, and found the
id by the explicit calculation 8*r + c, but have sinced revised my solution
to calculate the id directly from the single binary number.

The second part has two solutions that I am aware of: my original solution, which
takes the infinite stream of seat ids, first ignoring those that no-one is in,
then ignoring those that someone is in, and arriving eventually at an unaccounted for
seat in the middle. The downside of this approach is that you'll want to
build up a lookup set for all occupied seats.

A nicer approach, which I have now implemented, is to observe that
`sum-of-all-seat-ids - sum-of-accounted-seat-ids == your-seat-id`,
and you can calulcate the `sum-of-all-seat-ids` term using the minimum and maximum
(a small extension of part 1) and Gauss' rule `sum(from a to b) = 1/2 * (b - a + 1) * (a + b)`. Then you can build up the other term by summing the list of ids.
A much more efficient solution spacially.