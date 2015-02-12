{-
A module to turn a netlist into a circuit in the format used by Solver
-}

module Parser

import Data.Fin

import Solver
import Lightyear.Strings
import Lightyear.Combinators
import Lightyear.Core

%default partial
%access private

natToFloat : Nat -> Float
natToFloat = fromInteger . toIntegerNat

-- Convert a list of digits to a Nat
finsToNat : List (Fin 10) -> Nat
finsToNat = finsToNat' 0 where
  finsToNat' : Nat -> List (Fin 10) -> Nat
  finsToNat' acc []        = acc
  finsToNat' acc (x :: xs) = finsToNat' (acc * 10 + finToNat x) xs

-- Convert a two lists of digits to Float
finsToFloat : List (Fin 10) -> List (Fin 10) -> Float
finsToFloat whole fraction = let w = finsToNat whole
                                 f = finsToNat fraction
  in natToFloat w + (natToFloat f / (natToFloat $ length fraction))

float : Parser Float
float = do
  whole <- many digit   -- digit returns a Fin 10
  fraction <- opt (char '.' $> many digit)
  case fraction of
    Just fs => pure $ finsToFloat whole fs
    Nothing => pure $ finsToFloat whole []

nat : Parser Nat
nat = many digit >>= pure . finsToNat

resistor : (n : Nat) -> Parser (Component n)
resistor nodes = token "r" >! do
  r <- lexeme float
  n <- lexeme nat
  m <- lexeme nat
  case (natToFin n nodes, natToFin m nodes) of
    (Just f1, Just f2) => pure $ Resistor r (f1, f2)
    _                  => fail "Node out of bounds"

currentSource : (n : Nat) -> Parser (Component n)
currentSource nodes = token "i" >! do
  i <- lexeme float
  n <- lexeme nat
  case natToFin n nodes of
    Just f  => pure $ Current i f
    Nothing => fail "Node out of bounds"

circuit : Parser (Circuit n)
circuit = ?circuit_rhs
