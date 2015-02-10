simulation
==========

This is a circuit simulator based on the SPICE algorithm and written in Idris.
The idea is to see how dependent types can help ensure correctness in this
context. Because Idris is in the early stages of development and its
performance is not great yet I haven't put any particular effort into making
the code efficient. When standard techniques for improving the efficiency of
Idris code start to emerge I will incorporate them.

The mathematical basis for the simulator is the `Linear` module which contains
definitions for solving linear systems of equations. Equations can be solved by
either Gaussian elimination of LU decomposition but Gaussian elimination should
be more efficient for most cases.

The definitions of datatypes and the code that converts a circuit into a system
of equations for the `Linear` module is in `Solver.idr`. Currently only current
sources and resistors are supported.

The `Parser` module currently has nothing in it but will eventually take
netlist files and create circuits for the `Solver` module.
