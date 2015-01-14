{-
A module for solving systems of equations in matrix form. It is possible to do
this by either LU decomposition or Gaussian elemination depending on which is
better in each situation. In most cases for this project Gaussian elimination
should be faster.
-}

module Linear

import Data.Fin
import Data.Matrix

-- CONSIDER ADDING A PROOF THAT MATRICES ARE LOWER/UPPER

%default total
%access private

-- A LUSystem is actually just a matrix which has been PLU factorized so it can
-- easily be applied to any result vector
abstract
record LUSystem : Nat -> Type where
  MkLUSystem : (lower : Matrix n n Float) ->
               (upper : Matrix n n Float) ->
               (pivot : Matrix n n Float) -> LUSystem n

-- Create a pivot matrix for a given coefficient matrix
pivotize : {n : Nat} -> Matrix n n Float -> Matrix n n Float
pivotize {n} rows = let pos = map (chooseRow rows) range
                    in foldl swapRows Id $ zip range pos where
  chooseRow : Vect n (Vect n Float) -> Fin n -> Fin n
  chooseRow rows i = foldl (\a,r =>
                     if r > i &&
                        index i (index r rows) > index i (index a rows)
                     then r else a) i range
  swapRows : Matrix n n Float -> (Fin n, Fin n) -> Matrix n n Float
  swapRows (rows) (a,b) = map (\i => if i == b then index a rows
                                     else if i == a then index b rows
                                     else index i rows) range

-- Find an LU decomposition for the matrix m. The helper function is asserted
-- total because the index goes from 1 to n. The recursion is not structurally
-- smaller but it will terminate.
decompose : {n : Nat} -> Matrix n n Float ->
                         (Matrix n n Float, Matrix n n Float)
decompose {n=Z}   _    = ([], [])
decompose {n=S k} rows =
  let u = map (\r => map (\c =>
          if r == 0 then index c (index FZ rows)
                    else 0) (range {n=S k})) (range {n=S k})
      l = map (\r => map (\c =>
          if c == 0 then (index c (index r rows)) / (index FZ (index FZ u))
                    else 0) (range {n=S k})) (range {n=S k})
  in decompose_help 1 rows (l, u) where
    decompose_help : {n : Nat} -> Nat -> Vect n (Vect n Float) ->
                                  (Matrix n n Float, Matrix n n Float) ->
                                  (Matrix n n Float, Matrix n n Float)
    decompose_help {n} i a (ol, ou) = assert_total $
      let u = map (\r => map (\c => if finToNat r == i && finToNat c >= i
                  then index c (index r a) -
                       sum (map (\k => index k (index r ol) *
                                       index c (index k ou)) range)
                  else index c (index r ou)) range) range
          l = map (\r => map (\c => if finToNat c == i && finToNat r >= i
                  then (index c (index r a) -
                       sum (map (\k => index k (index r ol) *
                                       index c (index k u)) range)) /
                       (index c (index c u))
                  else index c (index r ol)) range) range
      in if i + 1 == n then (l, u)
                       else decompose_help (i + 1) a (l, u)

-- Create a system by pivotizing and LU factorizing the given matrix
public
createLUSystemFromMatrix : Matrix n n Float -> LUSystem n
createLUSystemFromMatrix m = let p      = pivotize m
                                 (l, u) = decompose (p <> m)
                             in MkLUSystem l u p

-- A helper function for solveLower and solveUpper below. Both use the same
-- function but one folds from the left and one folds from the right.
solve_help : Vect n Float -> (Vect n Float, Float, Fin n) -> Vect n Float
solve_help acc (r, x, i) = replaceAt i ((x - (acc <.> r)) / index i r) acc

-- Given a lower triangular matrix and a result vector, find a solution, i.e.
-- if [solveLower L b = x] then [L </> x = b].
solveLower : {n : Nat} -> Matrix n n Float -> Vect n Float -> Vect n Float
solveLower {n} l b = foldl solve_help (replicate n 0) $ zip3 l b range

-- Given an upper triangular matrix and a result vector, find a solution such
-- that if [solveUpper U y = x] then [U </> x = y].
solveUpper : {n : Nat} -> Matrix n n Float -> Vect n Float -> Vect n Float
solveUpper {n} u y = foldr (flip solve_help) (replicate n 0) $ zip3 u y range

-- Solve the given system using the given result vector, i.e. [solve s b] finds
-- the vector [x] such that (for [s = createLUSystemFromMatrix A])
-- [A </> x = b]. First we solve [L </> y = P </> b] for [y] and then use that
-- to find [x] such that [U </> x = y].
public
luSolve : LUSystem n -> Vect n Float -> Vect n Float
luSolve s b = let y = solveLower (lower s) ((pivot s) </> b)
              in solveUpper (upper s) y

-- Perform Gaussian elimination to get an upper triangular matrix
eliminate : Matrix n n Float -> Vect n Float ->
            (Matrix n n Float, Vect n Float)
eliminate a b = foldl eliminate_help (a, b) range where
  eliminate_help : (Matrix n n Float, Vect n Float) -> Fin n ->
                   (Matrix n n Float, Vect n Float)
  eliminate_help (a, b) i = unzip $ map (\r => if r <= i
    then (index r a, index r b)
    else let x = index i (index r a) / index i (index i a)
         in (zipWith (-) (index r a) (map (* x) (index i a)),
             index r b - x * index i b)) range

-- Solve a system by Gaussian elimination. This should be preferable in most
-- cases in the circuit simulator.
public
gaussianSolve : Matrix n n Float -> Vect n Float -> Vect n Float
gaussianSolve a b = let p       = pivotize a
                        (u, b') = eliminate (p <> a) (p </> b)
                    in solveUpper u b'
