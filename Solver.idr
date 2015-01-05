{-
This module takes a circuit as input (in an internal representation) and solves
the circuit (for the node voltages, after which the currents are easy to find).
-}

module Solver

import Linear

%default total
%access private

-- A component is a two terminal device. Other devices are represented using
-- models based on these.
data Component = Resistor Float (Nat, Nat)  -- resistance
               | Current Float (Nat, Nat)   -- current

-- A circuit is represented as a list of nodes (implicit, the nodes are just
-- numbered from 0 to n-1) where each node is connected to a list of
-- components. Components may exist in multiple copies, but this allows fast
-- retrieval of every component connected to a node.
Circuit : Nat -> Type
Circuit n = Vect n (List Component)

-- Convert a circuit to a system of equations (in matrix-vector form). The
-- number of equations is one less than the number of nodes.
circuitToSystem : Circuit (S n) -> (Matrix n n Float, Vect n Float)
circuitToSystem = unzip . map nodeToEquation . tail where
  nte_help : (Vect n Float, Float) -> Component -> (Vect n Float, Float)
  nte_help = ?nte_help_rhs
  nodeToEquation : {n : Nat} -> List Component -> (Vect n Float, Float)
  nodeToEquation {n} = foldl nte_help (replicate n 0, 0)

-- Take a circuit and find the voltages at each node
solveCircuit : Circuit (S n) -> Vect n Float
solveCircuit = (uncurry gaussianSolve) . circuitToSystem

{-              1
   1+------+--/\/\/--+2   [Current 2 (0, 1), Resistor 1 (0, 3),
    |     /-\        |     Resistor 1 (0, 4)]
    |  1A |||        |    [Current 2 (0, 1), Resistor 1 (1, 2),
   /^\    \v/   1    |     Current 1 (1, 3)]
2A |||    3+--/\/\/--+    [Resistor 1 (1, 2), Resistor 1 (2, 3),
   \_/     \         \     Resistor 1 (2, 4)]
    |      / 1       / 1  [Current 1 (1, 3), Resistor 1 (2, 3),
    |      \    1    \     Resistor 1 (3, 4)]
   0+------+--/\/\/--+4   [Resistor 1 (2, 4), Resistor 1 (0, 4)]


          2
   1+---/\/\/---+2   (v1 - v2) / 2 = 1 ==> v1 / 2 - v2 / 2 = 1
    |           |    v2 / 2 = (v1 - v2) / 2 ==> -v1 / 2 + v2 = 0
    |           /
   /^\          \      ==> [1/2  -1/2][v1] = [1]
1A |||          / 2        [-1/2    1][v2]   [0]
   \_/          \      ==> [v1 v2] = [4 2]
    |           /
    |           |
   0+-----------+


          1
   1+---/\/\/---+---------+2   (v1 - v2) / 1 = 1 ==> v1 - v2 = 1
    |           |         |    (v2 / 1) + (v2 / 2) = (v1 - v2) / 1
    |           /         /              ==> -v1 + 5/2 v2 = 0
   /^\          \         \
1A |||          / 2       / 1    ==> [1   -1][v1] = [1]
   \_/          \         \          [-1 5/2][v2]   [0]
    |           /         /      ==> [v1 v2] = [5/3  2/3]
    |           |         |
   0+-----------+---------+


          1k     2
    1+---/\/\/---+---------+   (v1 - v2) / 1000 = 0.001
     |           |         |      ==> (v1 / 1000) - (v2 / 1000) = 1/1000
     |           |         /   (v1 - v2) / 1000 + 1/1000 = v2 / 100
    /^\         /^\        \      ==> (v1 / 1000) - (11 v2 / 1000) = -1/1000
1mA |||     1mA |||    100 /
    \_/         \_/        \      ==> [1/1000  -1/1000][v1] = [ 1/1000]
     |           |         /          [1/1000 -11/1000][v2] = [-1/1000]
     |           |         |      ==> [v1  v2] = [1.2  0.2]
    0+-----------+---------+


             1
    +--------+---------+    (v1 - v2) / 1 + (v1 - v3) / 1 = 1
    |        \         \                   ==> 2 v1 - v2 - v3 = 1
    |      1 /       1 /    (v1 - v2) / 1 - v2 / 2 - (v2 - v3) / 2 = 0
   /^\       \    2    \                   ==> v1 - 2 v2 + v3 / 2 = 0
1A |||      2+--/\/\/--+3   (v1 - v3) / 1 + (v2 - v3) / 2 - v3 / 4 = 0
   \_/       \         \                   ==> v1 + v2 / 2 - 7 v3 / 4 = 0
    |      2 /       4 /          [2 -1  -1   ][v1]   [1]     [v1]   [13/7]
    |        \         \      ==> [1 -2  0.5  ][v2] = [0] ==> [v2] = [9/7 ]
   0+--------+---------+          [1 0.5 -1.75][v3]   [0]     [v3]   [10/7]
-}
