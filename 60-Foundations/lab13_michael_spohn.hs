-- Lab 13: Partitions

import Data.List (sort)

-- Sets of Ints as lists. Invariant: sets as lists are always normalized
type Set = [Int]

-- Relations on sets of Ints. Invariant: relations are normalized
type Rel = [(Int,Int)]

-- Universe (your code should work with any non-empty universe)
u :: Set
u = [1..8]

-- Mynub: remove adjacent duplicate elements in a list. For example,
-- mynub [3,4,4,4,2,2,4,1] = [3,4,2,4,1]
mynub :: Eq a => [a] -> [a]
mynub [] = []
mynub (x:[]) = [x]
mynub (x:xx:xs)
         | x == xx   = mynub (x:xs)
         | otherwise = x : mynub (xx:xs)

-- Normalize: sort and remove duplicates
norm :: Ord a => [a] -> [a]
norm = mynub . sort


-- A partitition of u is a set of blocks, each of which is a set of elements
-- of u, which satisfies certain conditions (complete, nontrivial, disjoint).
-- For example, here is the partitition of u corresponding to equivalence mod 3:
eqmod3_part :: [[Int]]
eqmod3_part = [[1,4,7], [2,5,8], [3,6]]


-- Write a function, part, that tests whether a list of lists is a partition
-- of u. You may assume that each element of bs is a set (i.e., is normalized)
-- and bs is itself a set
nontrivial :: [[Int]] -> Bool
nontrivial bs = or [not(null xs) | xs <- bs]

complete :: [[Int]] -> Bool
complete bs = and [or [elem a xs | xs <- bs] | a <- u]

disjoint :: [[Int]] -> Bool
disjoint bs = and [and [not(elem x ys) | x <- xs] | xs <- bs, ys <- bs, ys /= xs]

part :: [[Int]] -> Bool
part bs = (nontrivial bs) && (complete bs) && (disjoint bs)


-- Write a function, eq2part, that takes an equivalence relation on u as input
-- and returns the associated partition of u. You can assume that the input is
-- really an equivalence relation on u, and you should insure that the output
-- has the same invariants assumed by part above.
eq2part :: Rel -> [[Int]]
eq2part rs = mynub [[b | (a,b) <- rs, a == x] | (x,y) <- rs]


-- Write a function part2eq that takes a partition of u as input and returns
-- the associated equivalence relation on u. You can assume that the argument
-- is really a partition of u (and satisfies the same invariants)
part2eq :: [[Int]] -> Rel
part2eq bs = [(a,b) | xs <- bs, a <- xs, b <- xs]


-- Apply a function as a list of pairs. Assume that the argument is in the
-- domain of the function. For example, app [(1,3),(2,5),(3,2)] 2 = 5
app :: Rel -> Int -> Int
app rs a = undefined

-- Write a function, kernel, that takes a function on u as input (given as
-- a list of pairs), and returns the associated equivalence relation on u.
-- You can assume that the input is really a function on u.
kernel :: [(Int,Int)] -> Rel
kernel rs = undefined
