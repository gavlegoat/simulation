simulation
==========

This is a circuit simulator based on the SPICE algorithm and written in Idris.
The idea is to see how dependent types can help ensure correctness in this
context. Because Idris is in the early stages of development and its
performance is not great yet I haven't put any particular effort into making
the code efficient. When standard techniques for improving the efficiency of
Idris code start to emerge I will incorporate them.

So far the project consists only of a module for solving systems of equations.
This can be done either with Gaussian elimination or LU decomposition, but
Gaussian elimination should be faster in most situations that will arise in the
simulator.
