simulation
==========

This is a SPICE circuit simulator written in Idris. The idea is to see how
dependent types can help ensure correctness in this context. Because Idris is
in the early stages of development and its performance is not great yet I
haven't put any particular effort into making the code efficient. When standard
techniques for improving the efficiency of Idris code start to emerge I will
incorporate them.

So far the project consists only of a module for solving systems of equations,
which is done by LU factorization.
