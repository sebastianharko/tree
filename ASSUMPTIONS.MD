Assumptions
===========

1. Limited scalability since it obviously breaks
for very large number of rows (it works for 500 though).

2. The values in the nodes are reasonable numbers  (e.g, we won't 
have to handle very large numbers - no BigInt etc.).

3. Assuming that a 'single pass' solution (one that does not build
a 'Tree' structure after reading the input) is not useful.