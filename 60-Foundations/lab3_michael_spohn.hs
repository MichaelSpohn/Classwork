-- CSci 60 Lab 3
-- Michael Spohn 110360841


---------------- Solution to Part 2 of Lab 2

euclid :: Integer -> Integer -> Integer
euclid a 0 = a
euclid a b = euclid b (mod a b)

extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD a 0 = (a, 1, 0)
extGCD a b = let (q, r) = divMod a b
                 (d, m, n) = extGCD b r
             in (d, n, m-q*n)


---------------- Part 1: List recursions

-- Read Chapters 5 - 6 of the tutorial (the last chapters I will assign).
-- Using what you read in Ch. 5, provide recursive definitions for the
-- functions below.  Note: each "primed" function should do the same as
-- the "unprimed" Prelude function of the same name.

-- Minimum element in a list, with an error on the empty list
minimum' :: Ord a => [a] -> a
minimum' [] = error "minimum of empty list"
minimum' [x] = x
minimum' (x:xs)
    | x < minTail = x
    | otherwise = minTail
    where minTail = minimum' xs

-- Sum of the elements in a list
sum' :: Num a => [a] -> a
sum' [] = error "sum of empty list"
sum' [x] = x
sum' (x:xs) = sum' xs + x

-- The result of dropping the first n elements of a list
drop' :: Int -> [a] -> [a]
drop' a [] = []
drop' 0 xs = xs
drop' a (x:xs) = drop' (a-1) xs

-- Zip together three parallel lists
zip3' :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3' _ _ [] = []
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z):zip3' xs ys zs

-- checks if something is not in a list
notElem' :: Eq a => a -> [a] -> Bool
notElem' a [] = True
notElem' a (x:xs)
    | a == x    = False
    | otherwise = a `notElem'` xs

-- "and" together all Booleans in a list (true if all are true)
and' :: [Bool] -> Bool
and' [] = error "empty list"
and' [x] = x
and' (x:xs)
    | x == False = False
    | otherwise  = and' xs

-- "or" together all Booleans in a list (true if at least one is true)
or' :: [Bool] -> Bool
or' [] = error "empty list"
or' [x] = x
or' (x:xs)
    | x == True = True
    | otherwise = or' xs


---------------- Part 2: Prime factorization (implementing the FTA)

-- Two definitions of the infinite list of primes
primes_slow = sieve [2..] where
  sieve (p:xs) = p : sieve [x | x <- xs, rem x p /= 0]

primes = 2 : primesFrom3 where 
    primesFrom3 = sieve [3,5..] 9 primesFrom3
    sieve (x:xs) b ~ps@(p:q:_)
      | x < b     = x : sieve xs b ps
      | otherwise =     sieve [x | x <- xs, rem x p /= 0] (q^2) (tail ps)

-- Return the prime factorization of n, using factor
primeFact :: Integer -> [Integer]
primeFact n = factor n primes

-- Factor n using factors in ps, using the algorithm described in class.
-- If factor n ps = xs, then xs is a subset of ps and product xs == n.
factor :: Integer -> [Integer] -> [Integer]
factor 0 _    = []
factor 1 _    = []
factor _ []   = []
factor n (p:ps)
      | mod n p == 0 = p : factor (div n p) (p:ps)
      | otherwise    = factor n ps