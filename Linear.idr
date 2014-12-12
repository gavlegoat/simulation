{-
A module for solving linear systems of equations. This serves as the
mathematical basis for the simulator. We use an LU decomposition with a pivot
matrix, i.e. a system is a set of matricese P, L, and U, such that

> PA=LU

The pivot matrix is defined in order to keep the numerical computations more
accurate by reducing the amount of division by small numbers needed.

Then to solve we use

> Ax=b => (P^{ -1} LU)x=b => LUx=Pb => Ly=Pb and Ux=y

Note that because the matrices we are interested in are all square they can
all be LUP factorized.
-}

module Linear

-- Note Data.Matrix is not in the latest release at time of writing (0.9.15.1)
-- so the Matrix.idr file was copied from the repository and compiled manually
import Data.Matrix

%default total
%access private

-- A System is actually just a matrix which has been PLU factorized so it can
-- easily be applied to any result vector
abstract
record System : Nat -> Type where
  MkSystem : (lower : Matrix n n Float) ->
             (upper : Matrix n n Float) ->
             (pivot : Matrix n n Float) -> System n

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
createSystemFromMatrix : Matrix n n Float -> System n
createSystemFromMatrix m = let p      = pivotize m
                               (l, u) = decompose (p <> m)
                           in MkSystem l u p

-- A helper function for solveLower and solveUpper below. Both use the same
-- function but one folds from the left and one folds from the right.
solve_help : Vect n Float -> (Vect n Float, Float, Fin n) -> Vect n Float
solve_help acc (r, x, i) = replaceAt i ((x - (acc <.> r)) / index i r) acc

-- Given a lower triangular matrix and a result vector, find a solution, i.e.
-- if [solveLower L b = x] then Lx=b.
solveLower : {n : Nat} -> Matrix n n Float -> Vect n Float -> Vect n Float
solveLower {n} l b = foldl solve_help (replicate n 0) $ zip3 l b range

-- Given an upper triangular matrix and a result vector, find a solution such
-- that if [solveUpper U y = x] then Ux=y.
solveUpper : {n : Nat} -> Matrix n n Float -> Vect n Float -> Vect n Float
solveUpper {n} u y = foldr (flip solve_help) (replicate n 0) $ zip3 u y range

-- Solve the given system using the given result vector, i.e. [solve s b] finds
-- the vector x such that (for [s = createSystemFromMatrix A]) Ax=b. First we
-- Ly=Pb for y and then use that to find x such that Ux=y.
public
solve : System n -> Vect n Float -> Vect n Float
solve s b = let y = solveLower (lower s) ((pivot s) </> b)
            in solveUpper (upper s) y
