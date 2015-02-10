{-
A module to turn a netlist into a circuit in the format used by Solver
-}

module Parser

import Data.Fin

import Solver
import Lightyear

%default total
%access private

float : Parser Float
float = ?float_rhs

nat : Parser Nat
nat = ?nat_rhs

resistor : Nat -> Parser Component
resistor nodes = token "r" >! do
  r <- float
  n <- nat
  m <- nat
  case (natToFin n nodes, natToFin m nodes) of
    (Just f1, Just f2) => pure $ Resistor r (f1, f2)
    _                  => satisfy (const False) <?> "Node out of bounds"

currentSource : Parser Component
currentSource nodes = token "i" >! do
  i <- float
  n <- nat
  case natToFin n nodes of
    Just f  => pure $ Current i f
    Nothing => satisfy (const False) <?> "Node out of bounds"

circuit : Parser Circuit
circuit = ?circuit_rhs
