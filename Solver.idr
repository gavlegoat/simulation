{-
This module takes a circuit as input (in an internal representation) and solves
the circuit (for the node voltages, after which the currents are easy to find).
-}

module Solver

import Data.Fin
import Data.Matrix

import Linear

%default total
%access private

-- A component is a two terminal device. Other devices are represented using
-- models based on these. Each component also knows how many nodes are in the
-- whole circuit (I'll try to find a better way to deal with this later).
public
data Component : (n : Nat) -> Type where
  Resistor : Float -> (Fin n, Fin n) -> Component n   -- resistance and nodes
  Current : Float -> Fin n -> Component n   -- current, node current flows into

-- A circuit is represented as a list of nodes (implicit, the nodes are just
-- numbered from 0 to n-1) where each node is connected to a list of
-- components. Components may exist in multiple copies, but this allows fast
-- retrieval of every component connected to a node.
public
Circuit : Nat -> Type
Circuit n = Vect (S n) (List (Component (S n)))

-- Decrement a Fin n and strengthen the bound
predAndStrengthen : Fin (S (S n)) -> Fin (S n)
predAndStrengthen FZ     = FZ
predAndStrengthen (FS n) = n

-- Convert a circuit to a system of equations (in matrix-vector form). The
-- number of equations is one less than the number of nodes.
public
circuitToSystem : Circuit (S n) -> (Matrix (S n) (S n) Float, Vect (S n) Float)
circuitToSystem = unzip . map nodeToEquation . tail .
                  Data.VectType.Vect.zip range where
  nte_help : Fin (S (S n)) -> (Vect (S n) Float, Float) ->
             Component (S (S n)) -> (Vect (S n) Float, Float)
  nte_help l (xs, x) (Resistor r (m, o)) =
    let sm = predAndStrengthen m
        so = predAndStrengthen o
    in if m == FZ then (updateAt so (\x => x - (1 / r)) xs, x)
       else if o == FZ then (updateAt sm (\x => x - (1 / r)) xs, x)
       else if o > m
         then (updateAt so (+ (1 / r)) (updateAt sm (\x => x - (1 / r)) xs), x)
         else (updateAt sm (+ (1 / r)) (updateAt so (\x => x - (1 / r)) xs), x)
  nte_help l (xs, x) (Current i m) = if l == m then (xs, x + i)
                                               else (xs, x - i)
  nodeToEquation : {n : Nat} -> (Fin (S (S n)), List (Component (S (S n)))) ->
                   (Vect (S n) Float, Float)
  nodeToEquation {n} (m, xs) = foldl (nte_help m) (replicate (S n) 0, 0) xs

-- Take a circuit and find the voltages at each node
public
solveCircuit : Circuit (S n) -> Vect (S n) Float
solveCircuit = (uncurry gaussianSolve) . circuitToSystem
