# Day 7 

The name of the game today is graphs. The relations between
bag styles can be seen as edges, where `a -- 3 --> b`
signifies that a bag of style `a` must contain 3 bags
of style `b`.

The first part of the question asks for the bag styles
that somehow involve a `shiny gold` one, which in the
lingo is asking "how many bag styles `c` begin a path `c -- n --> shiny gold` where `n` /= 0?". My solution
directly asks the `Data.Graph` module to answer that question.


The second part is asking to traverse the graph starting at `shiny gold` and treating an edge with the label `n`
as "I want `n` multiplied by however many nodes you encounter afterwards, doing the same thing at every later edge".

For this to terminate we cannot have cycles in the graph -- since this would mean that for instance every `matte blue` bag should contain a `matte blue bag`, should contain ...