---- CSci 119, Lab 1 ----
----  Michael Spohn  ----

-- Note: you will replace all instances of "undefined" below with your answers.


---- Boolean operators

-- The following code tests whether "and" is commutative:
bools = [True, False]
and_commutes = and [(p && q) == (q && p) | p <- bools, q <- bools]

-- Write similar defintions that test whether "or" is commutative,
-- "and" and "or" are associative, "and" distributes over "or",
-- "or" distributes over "and"
or_commutes = and [(p || q) == (q || p) | p <- bools, q <- bools]
and_assoc   = and [(p && (q && r)) == ((p && q) && r) | p <- bools, q <- bools, r <- bools]
or_assoc    = and [(p || (q || r)) == ((p || q) || r) | p <- bools, q <- bools, r <- bools]
and_dist    = and [(p && (q || r)) == ((p && q) || (p && r)) | p <- bools, q <- bools, r <- bools]
or_dist     = and [(p || (q && r)) == ((p || q) && (p || r)) | p <- bools, q <- bools, r <- bools]
          
-- The exclusive-or operation on Bool in Haskell is equivalent to /=.
-- Test the properties of this operation (commutativity, associativity,
-- distributivity over and+or, and distributivity of and+or over it)
-- using the same method as above
xor_commutes = and [(p /= q) == (q /= p) | p <- bools, q <- bools]
xor_assoc    = and [(p /= (q /= r)) == ((p /= q) /= r) | p <- bools, q <- bools, r <-bools]
xor_dist_and = and [(p /= (q && r)) == ((p /= q) && (p /= r)) | p <- bools, q <- bools, r <- bools]
xor_dist_or  = and [(p /= (q || r)) == ((p /= q) || (p /= r)) | p <- bools, q <- bools, r <- bools]
and_dist_xor = and [(p && (q /= r)) == ((p && q) /= (p && r)) | p <- bools, q <- bools, r <- bools]
or_dist_xor  = and [(p || (q /= r)) == ((p || q) /= (p || r)) | p <- bools, q <- bools, r <- bools]
               
-- The implication operator on Bool in Haskell is equivalent to <=.
-- Check whether implication is associative or commutative:
assoc_imp = and [(p <= (q <= r)) == ((p <= q) <= r) | p <- bools, q <- bools, r <- bools]
comm_imp  = and [(p <= q) == (q <= p) | p <- bools, q <- bools]


----- Evaluating statements involving quantifiers

-- Assume that the universe of discourse is the set {1,2,3,4,5,6,7,8},
-- that is, that the word "number" temporarily means 1, 2, ..., 8.
-- Your solutions to the problems below should work no matter what
-- finite list of integers u is. For example, u = [5, 2, 17, 58, 21].

u = [1..8]

-- Translate each of the following statements first, in a comment, into a
-- logical statement involving forall, exists, and, or, imp, and not,
-- and then into Haskell code that checks ("brute force") whether
-- the statement is true. I'll work one example.

-- 1. "Every number that's greater than 2 is greater than 1"
-- A: forall n, (n > 2) imp (n > 1)
prob1_1 = and [(n > 2) <= (n > 1) | n <- u]   -- direct translation
prob1_2 = and [n > 1 | n <- u, n > 2]         -- using bounded quantifier

-- 2. Every number is either greater than 1 or less than 2
-- A: forall n, (n > 1) or (n < 2)
prob2 = and [(n > 1) || (n < 2) | n <- u]

-- 3. Every two numbers are comparable with <= (i.e., either one is <=
--    the other or vice-versa)
-- A: forall m and forall n, (m <= n) or (n <= m)
prob3 = and [(m <= n) || (n <= m) | m <- u, n <- u]

-- 4. There is an odd number greater than 4
-- A: exists odd n, (n > 4)
prob4 = or [(n > 4) | n <- u, odd n]

-- 5: There are two odd numbers that add up to 10
-- A: exists odd m and exists odd n, (m + n = 10)
prob5 = or [(m + n) == 10 | m <- u, n <- u, odd m, odd n]

-- 6. For every odd number, there is a greater even number (use the Haskell
--    predicates odd, even :: Integral a => a -> Bool)
-- A: forall odd m exists even n, (m < n)
prob6 = and [or [(m < n) | m <- u, n <- u, odd m, even n]]

-- 7. For every even number, there is a greater odd number
-- A: forall even m exists odd n, (m < n)
prob7 = and [or [(m < n) | m <- u, n <- u, even m, odd n]]

-- 8. There are two odd numbers that add up to 6
-- A: exists odd m and exists odd n, (m + n = 6)
prob8 = or [(m + n) == 6 | m <- u, n <- u, odd m, odd n]

-- 9. There is a number that is at least as large as every number
--    (i.e., according to >=)
-- A: exists m forall n, (m >= n)
prob9 = or [and [m >= n | m <- u] | n <- u]

-- 10. For every number, there is a different number such that there are no
--    numbers between these two.
-- A: forall m exists n, m /= n and not exists o, (m < o < n)
prob10 = and [or [or [not ((m < o) && (o < n)) || not ((n < o) && (o < m)) | m <- u, n <- u, o <- u, m /= n]]]