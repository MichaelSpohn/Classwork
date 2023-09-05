-- Lab 14: Partial Orders

type Set = [Int]
type Rel = [(Int,Int)]

u :: Set
u = [1..8]


-- For your information, here are tests for reflexivity, symmetry,
-- transitivity, and antisymmetry, along with a test for partial order
refl :: Rel -> Bool
refl ps = and [elem (x,x) ps | x <- u]

symm :: Rel -> Bool
symm ps = and [elem (y,x) ps | (x,y) <- ps]

trans :: Rel -> Bool
trans ps = and [elem (x,z) ps | (x,y1) <- ps, (y2,z) <- ps, y1 == y2]

antisymm :: Rel -> Bool
antisymm ps = and [not (elem (y,x) ps) | (x,y) <- ps, x /= y]

poset :: Rel -> Bool
poset ps = refl ps && trans ps && antisymm ps

-- Helper Function: convert
convert :: Maybe Int -> Bool
convert (Just x) = True
convert Nothing  = False


-- A few example partial orders on u (develop some more yourself!)

eq = [(i,i) | i <- u]
leq = [(i,j) | i <- u, j <- u, i <= j]
divis = [(i,j) | (i,j) <- leq, j `mod` i == 0]
two_by_four = [(i,j) | (i,j) <- leq, even i <= even j]


---- Lab 14 begins here ----

-- Note: all functions below can assume that poset ps == True (you don't need
-- to check this).


-- Least element of a subset of a poset (if it exists)
-- least ps xs == Just a, if a is the least element of xs
-- least ps xs == Nothing, if xs has no least element
least :: Rel -> Set -> Maybe Int
least ps xs = find_least xs where
  find_least [] = Nothing
  find_least (a:ys) | and [elem (a,x) ps | x <- xs] = Just a
                    | otherwise = find_least ys

-- Greatest element of a subset of a poset (if it exists)
-- greatest ps xs == Just a, if a is the greatest element of xs
-- greatest ps xs == Nothing, if xs has no greatest element
greatest :: Rel -> Set -> Maybe Int
greatest ps xs = find_greatest xs where
  find_greatest [] = Nothing
  find_greatest (a:ys) | and [elem (x,a) ps | x <- xs] = Just a
                       | otherwise = find_greatest ys

-- Least upper bound of two elements of a poset (if it exists)
lub :: Rel -> Int -> Int -> Maybe Int
lub ps x y = case [a | a <- u, elem (x,a) ps, elem (y,a) ps] of
                []     -> Nothing
                (a:as) -> Just a

-- Greatest lower bound of two elements of a poset (if it exists)
glb :: Rel -> Int -> Int -> Maybe Int
glb ps x y = case [a | a <- u, elem (a,x) ps, elem (a,y) ps] of
                []     -> Nothing
                (a:as) -> Just a

-- linear ps == True if ps is a linear (aka total) order
linear :: Rel -> Bool
linear ((a,b):ps) = and [and [elem (a,b) ps || elem (b,a) ps]]

-- lattice ps == True if ps is a lattice
lattice :: Rel -> Bool
lattice ps = undefined

-- Coverings of an element in a poset
coverings :: Rel -> Int -> Set
coverings ps x = undefined

-- Minimal elements of a subset of a poset
minimal :: Rel -> Set -> Set
minimal ps xs = [a | a <- xs, and [(elem (b,a) ps) <= (b == a) | b <- xs]]

-- Maximal elements of a subset of a poset
maximal :: Rel -> Set -> Set
maximal ps xs = [a | a <- xs, and [(elem (a,b) ps) <= (a == b) | b <- xs]]

-- Topological sorting of a subset of a poset
topsort :: Rel -> Set -> Set
topsort ps [] = []
topsort ps xs = let ys = minimal ps xs
                in ys ++ topsort ps (removeduplicate xs ys) where
                  removeduplicate [] ys = []
                  removeduplicate (x:xs) ys | elem x ys = removeduplicate xs ys
                                            | otherwise = x : removeduplicate xs ys