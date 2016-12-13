-- "very strong convention in Haskell to never type class contraints
-- in declarations... put them into function type declarations" (121)
-- Here, `Vector` has a parameterized type `a`
-- The type of `Vector` is `Vector a` not `Vector a a a`
data Vector a = Vector a a a deriving (Show)

-- In the function declaration, we add a type constraint on `a` to be of
-- type class `Num`, now the `Vector` type is filled with `Num`'s
vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` s = Vector (i*s) (j*s) (k*s)

dot :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dot` (Vector l m n) = (i*l) + (j*m) + (k*n)

