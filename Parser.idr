{-
A module to turn a netlist into a circuit in the format used by Solver

Netlist file format:
  Each line has a single component. The first character in the line identifies
  the component (e.g. r for resistor) and the rest of the line gives
  component-dependent paramters. At the momen there is no support for comments
  The nodes must be numbered starting from zero with no numbers skipped.

Example:              1+---/\/\/---+------+2
                       |   1 Ohm   |      |
  i 1.0 0 1  ___|\    /^\          \ 2    \ 2
  r 1.0 1 2  |    \   ||| 1A       / Ohm  / Ohm
  r 2.0 2 0  |__  /   \_/          \      \
  r 2.0 2 0     |/     |           |      |
                      0+-----------+------+
-}

module Parser

import Data.Fin
import Lightyear.Strings
import Lightyear.Combinators
import Lightyear.Core
import Solver

%default partial
%access private

-- Parse a natural number, probably a node label
nat : Parser Nat
nat = do
  n <- integer
  if (n < 0) then fail "Unexpected negative number" else pure (fromInteger n)

-- These two functions are lifted straight out of the JSON example in the
-- lightyear repo except that in the repo they're called `scientificToFloat`
-- and `parseScientific`.
makeFloat : Integer -> Integer -> Float
makeFloat b e = fromInteger b * exp where
  exp = if e < 0 then 1 / pow 10 (fromIntegerNat (- e))
                 else pow 10 (fromIntegerNat e)

-- Parse a float, e.g. a resistance or current value
float : Parser Float
float = do sign <- maybe 1 (const (-1)) `map` opt (char '-')
           digits <- some digit
           hasComma <- isJust `map` opt (char '.')
           decimals <- if hasComma then some digit else pure $ Prelude.List.Nil
           hasExponent <- isJust `map` opt (char 'e')
           exponent <- if hasExponent then integer else pure 0
           pure $ makeFloat (sign * fromDigits (digits ++ decimals))
                            (exponent - cast (length decimals))
  where fromDigits : List (Fin 10) -> Integer
        fromDigits = foldl (\a,b => 10 * a + cast b) 0

-- Parse any component that has one value, the third argument is a constructor
oneParameterComponent : (n : Nat) -> String ->
                        (Float -> (Fin n, Fin n) -> Component n) ->
                        Parser (Component n)
oneParameterComponent nodes s f = token s >! do
  v <- lexeme float
  n <- lexeme nat
  m <- lexeme nat
  case (natToFin n nodes, natToFin m nodes) of
    (Just f1, Just f2) => pure $ f v (f1, f2)
    _                  => fail "Node out of bounds"

-- Parse a resistor line including endpoints and resistance
resistor : (n : Nat) -> Parser (Component n)
resistor nodes = oneParameterComponent nodes "r" Resistor

-- Parse a current source line includeing endpoints and current
currentSource : (n : Nat) -> Parser (Component n)
currentSource nodes = oneParameterComponent nodes "i" Current

-- Get a component of any kind
component : (n : Nat) -> Parser (Component n)
component n = resistor n
          <|> currentSource n

-- Parse several components separated by newlines
manyComponents : (n : Nat) -> Parser (List (Component n))
manyComponents n = sepBy (component n) (char '\n' <|> string "\r\n")

-- Parse a circuit. The value of n is determined by a prepass over the file
-- which finds the number of nodes.
circuit : (n : Nat) -> Parser (Circuit n)
circuit n = do components <- manyComponents (S n)
               pure $ map (componentsByNode components) range
  where componentsByNode : List (Component m) -> Fin m -> List (Component m)
        componentsByNode cs f = foldl (\l,c => case c of
          Resistor _ (p, q) => if p == f || q == f then
                                c :: l else l
          Current _ (p, q) => if p == f || q == f then
                               c :: l else l) [] cs

-- Get the number of nodes in the circuit by parsing an arbitrary, large,
-- number of components and finding the maximum node number.
numNodes : String -> Nat
numNodes s = case parse (manyComponents 100) s of
               Left s   => Z
               Right cs => foldl maximum 0 (map maxNode cs)
  where maxNode : (Component n) -> Nat
        maxNode (Resistor _ (n1, n2)) = maximum (finToNat n1) (finToNat n2)
        maxNode (Current _ (n1, n2))  = maximum (finToNat n1) (finToNat n2)

-- The main parser function
parseCircuit : String -> Either String (n : Nat ** Circuit n)
parseCircuit s = case numNodes s of
                   Z => Left "Parser.idr:111: This should never happen"
                   n => Right (n ** parse (circuit n) s)
