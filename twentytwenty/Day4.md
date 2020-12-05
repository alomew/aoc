# Day 4

The largest part of my time was spent debating how rigorous a representation
of a passport I should design. The issue is really that the two parts seem
to beg slightly different styles. In the end, especially since the puzzle suggests
these documents will resurface, I thought it best to try and apply a fair amount of rigour --- I am sure more will be required later.

As a more general comment, I have decided that my shoe-horn approach to a main method works fine for the final parts, but not too well for small tests you might want to write as you go, especially when the questions you are asking are not just "does the solution work for the single toy data". Thus I have now moved to an HSpec suite. More typing, but hopefully it'll prove a more responsive approach.