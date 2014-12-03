{-
A module for solving linear systems of equations. This serves as the
mathematical basis for the simulator. We use an LU decomposition with a pivot
matrix, i.e. a system is a set of matricese P, L, and U, such that

> PA=LU

The pivot matrix is defined in order to keep the numerical computations more
accurate by reducing the amount of division by small numbers needed in the
following way (described here imperatively):

ident = identity(numcols(m))
for i from 1 to numcols(m)
  max := m[i][i]
  maxrow := i
  for j from i+1 to numrows(m)
    if m[j][i] > max
      max := m[j][i]
      maxrow := j
    endif
  endfor
  swaprows(ident, i, maxrow)
endfor

For example
        /[1 3 5]\   [0 1 0]
pivotize|[2 4 7]| = [1 0 0]
        \[1 1 0]/   [0 0 1]
Then to solve we use

> Ax=b => (P^{ -1} LU)x=b => LUx=Pb => Ly=Pb and Ux=y

Note that because the matrices we are interested in are all square they can
all be LUP factorized.
-}

module Linear

-- Note Data.Matrix is not in the latest release at time of writing (0.9.15.1)
-- so the Matrix.idr file was copied from the repository and compiled
import Data.Matrix

%default total

-- A System is actually just a matrix which has been PLU factorized so it can
-- easily be applied to any result vector
public
record System : Nat -> Type where
  MkSystem : (lower : Matrix n n Float) ->
             (upper : Matrix n n Float) ->
             (pivot : Matrix n n Float) -> System n

-- Construct an nxn identity matrix
identity : (n : Nat) -> Matrix n n Float
identity n = map (\r => map (\c => if r == c then 1 else 0) range) range

-- Create a pivot matrix for a given coefficient matrix
pivotize : {n : Nat} -> Matrix n n Float -> Matrix n n Float
pivotize {n} rows = let ident = identity n
                        pos = map (chooseRow rows) range in
                    foldl swapRows ident $ zip range pos where
  chooseRow : Vect n (Vect n Float) -> Fin n -> Fin n
  chooseRow rows i = foldl (\a,r =>
                     if r > i &&
                        index i (index r rows) > index i (index a rows)
                     then r else a) i range
  swapRows : Matrix n n Float -> (Fin n, Fin n) -> Matrix n n Float
  swapRows (rows) (a,b) = map (\i => if i == b then index a rows
                                    else if i == a then index b rows
                                    else index i rows) range

-- A helper functiion for decompose (below). It is asserted total because it
-- recurses with the index going from 1 to n so the recursive case isn't
-- structurally smaller, but it will finish.
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

-- Find an LU decomposition for the matrix m
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
  in decompose_help 1 rows (l, u)

-- Create a system by pivotizing and LU factorizing the given matrix
public
createSystemFromMatrix : Matrix n n Float -> System n
createSystemFromMatrix m = let p      = pivotize m
                               (l, u) = decompose (p <> m)
                           in MkSystem l u p

-- Solve the given system using the given result vector, i.e. [solve s b] finds
-- the vector x such that (for [s = createSystemFromMatrix A]) Ax=b. First we
-- Ly=Pb for y and then use that to find x such that Ux=y.
solve : System n -> Vect n Float -> Vect n Float
solve s b = let y = solveLower (lower s) $ (pivot s) </> b
            in solveUpper (upper s) y where
  solveLower : Matrix n n Float -> Vect n Float -> Vect n Float
  solveLower = ?solveLower_rhs
  solveUpper : Matrix n n Float -> Vect n Float -> Vect n Float
  solveUpper = ?solveUpper_rhs

{-
solveLower:
[a11,0,  0  ] [y1]   [b1]     y1 = b1 / a11
[a21,a22,0  ] [y2] = [b2] ==> y2 = (b2 - a21*y1) / a22
[a31,a32,a33] [y3]   [b3]     y3 = (b3 - a31*y1 - a32*y2) / a33
        1   /      n-1          \
y_n = ---- | b_n -  ∑ a_ni * y_i |
      a_nn  \      i=1          /

solveUpper:
[a11,a12,a13] [x1]   [y1]     x1 = (y1 - x3*a13 - x2*a12) / a11
[0,  a22,a23] [x2] = [y2] ==> x2 = (y2 - x3*a23) / a22
[0,  0,  a33] [x3]   [y3]     x3 = y3 / a33
(n goes from highest to lowest)
        1   /       max           \
x_n = ---- | y_n -   ∑  a_ni * x_i |
      a_nn  \      i=n+1          /
-}
