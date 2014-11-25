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
-}
module Linear

%default total

-- Matrix of n rows by m columns, represented as a vector of rows
data Matrix : Nat -> Nat -> Type where
  MkMatrix : Vect n (Vect m Float) -> Matrix n m

-- A dot product on two vectors, used to implement matrixMult
dotProduct : Vect n Float -> Vect n Float -> Float
dotProduct u v = sum $ zipWith (*) u v

-- Extract a column from a matrix
column : Fin m -> Matrix n m -> Vect n Float
column c (MkMatrix rows) = map (index c) rows

-- Multiply two matrices
matrixMult : Matrix n m -> Matrix m o -> Matrix n o
matrixMult (MkMatrix a) b = MkMatrix $
  map (\r => map (\c => dotProduct (index r a) (column c b))
                 range) range

-- A System is actually just a matrix which has been PLU factorized so it can
-- easily be applied to any result vector
record System : Nat -> Type where
  MkSystem : (lower : Matrix n n) ->
             (upper : Matrix n n) ->
             (pivot : Matrix n n) -> System n

-- Construct an nxn identity matrix
identity : (n : Nat) -> Matrix n n
identity n = MkMatrix $ map (\r => map (\c => if r == c then 1 else 0)
                                       range) range

-- Create a pivot matrix for a given coefficient matrix
pivotize : {n : Nat} -> Matrix n n -> Matrix n n
pivotize {n} (MkMatrix rows) = let ident = identity n
                                   pos = map (chooseRow rows) range in
                               foldl swapRows ident $ zip range pos where
  chooseRow : Vect n (Vect n Float) -> Fin n -> Fin n
  chooseRow rows i = foldl (\a,r =>
                     if r > i &&
                        index i (index r rows) > index i (index a rows)
                     then r else a) i range
  swapRows : Matrix n n -> (Fin n, Fin n) -> Matrix n n
  swapRows (MkMatrix rows) (a,b) = MkMatrix $
                                   map (\i => if i == b then index a rows
                                              else if i == a then index b rows
                                              else index i rows) range

-- A helper functiion for decompose (below). It is asserted total because it
-- recurses with the index going from 1 to n so the recursive case isn't
-- structurally smaller, but it will finish.
decompose_help : {n : Nat} -> Nat -> Vect n (Vect n Float) ->
                 (Matrix n n, Matrix n n) -> (Matrix n n, Matrix n n)
decompose_help {n} i a (MkMatrix ol, MkMatrix ou) = assert_total $
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
  in if i + 1 == n then (MkMatrix l, MkMatrix u)
                   else decompose_help (i + 1) a (MkMatrix l, MkMatrix u)

-- Find an LU decomposition for the matrix m
decompose : {n : Nat} -> Matrix n n -> (Matrix n n, Matrix n n)
decompose {n=Z}   _               = (MkMatrix [], MkMatrix [])
decompose {n=S k} (MkMatrix rows) =
  let u = map (\r => map (\c =>
          if r == 0 then index c (index FZ rows)
                    else 0) (range {n=S k})) (range {n=S k})
      l = map (\r => map (\c =>
          if c == 0 then (index c (index r rows)) / (index FZ (index FZ u))
                    else 0) (range {n=S k})) (range {n=S k})
  in decompose_help 1 rows (MkMatrix l, MkMatrix u)

-- Create a system by pivotizing and LU factorizing the given matrix
public
createSystemFromMatrix : Matrix n n -> System n
createSystemFromMatrix m = let p      = pivotize m
                               (l, u) = decompose (matrixMult m p)
                           in MkSystem l u p
