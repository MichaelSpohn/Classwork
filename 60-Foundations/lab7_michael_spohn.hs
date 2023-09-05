-- CSci 60 Lab 7
-- Michael Spohn 110360841

bools = [True, False]

-- Similar to one of the answers from Lab 6, the following code exhaustively
-- checks whether not(P) \/ Q is equivalent to P -> Q.
not_equiv = and [(not p || q) == (p <= q) | p <- bools, q <- bools]


-- Write similar defintions that check each of the following equivalences:

-- P /\ Q = Q /\ P                           and is commutative
and_comm            = and [(p && q) == (q && p) | p <- bools, q <- bools]

-- P \/ Q = Q \/ P                           or is commutative
or_comm             = and [(p || q) == (q || p) | p <- bools, q <- bools]

-- P /\ (P -> Q) = P /\ Q
and_implies         = and [(p && (p <= q)) == (p && q) | p <- bools, q <- bools]

-- P -> (P -> Q) = P -> Q
implies_implies     = and [(p <= (p <= q)) == (p <= q) | p <- bools, q <- bools]

-- P /\ (Q /\ R) = (P /\ Q) /\ R             and is associative
and_asso            = and [(p && (q && r)) == (r && (p && q)) | p <- bools, q <- bools, r <- bools]

-- P \/ (Q \/ R) = (P \/ Q) \/ R             or is associative
or_asso             = and [(p || (q || r)) == (r || (p || q)) | p <- bools, q <- bools, r <- bools]

-- P /\ (Q \/ R) = (P /\ Q) \/ (P /\ R)      and distributes over or
and_or              = and [(p && (q || r)) == ((p && q) || (p && r)) | p <- bools, q <- bools, r <- bools]

-- P \/ (Q /\ R) = (P \/ Q) /\ (P \/ R)      or distributes over and
or_and              = and [(p || (q && r)) == ((p || q) && (p || r)) | p <- bools, q <- bools, r <- bools]

-- P -> (Q /\ R) = (P -> Q) /\ (P -> R)      implies distributes over and
implies_and         = and [(p <= (p && r)) == ((p <= q) && (p <= r)) | p <- bools, q <- bools, r <- bools]

-- (P \/ Q) -> R = (P -> R) /\ (Q -> R)
or_implies_and      = and [((p || q) <= r) == ((p <= r) && (q <= r)) | p <- bools, q <- bools, r <- bools]

-- P -> (Q -> R) = (P /\ Q) -> R
implies_and_implies = and [(p <= (q <= r)) == ((p && q) <= r) | p <- bools, q <- bools, r <- bools]


-- The exclusive-or (xor) operation is equivalent to the /= operator in Haskell
-- Which of the following properties of exclusive-or are true? Answer each by
-- supplying Haskell code to check.

-- xor is commutative (TRUE)
xor_comm    = and [(p /= q) == (q /= p) | p <- bools, q <- bools]

-- xor is associative (TRUE)
xor_asso    = and [(p /= (q /= r)) == (r /= (p /= q)) | p <- bools, q <- bools, r <- bools]

-- xor distributes over and (FALSE)
xor_and     = and [(p /= (q && r)) == ((p /= q) && (p /= r)) | p <- bools, q <- bools, r <- bools]

-- xor distributes over or (FALSE)
xor_or      = and [(p /= (q || r)) == ((p /= q) || (p /= r)) | p <- bools, q <- bools, r <- bools]

-- and distributes over xor (TRUE)
and_xor     = and [(p && (q /= r)) == ((p && q) /= (p && r)) | p <- bools, q <- bools, r <- bools]

-- or distributes over xor (FALSE)
or_xor      = and [(p || (p /= r)) == ((p || q) /= (p || r)) | p <- bools, q <- bools, r <- bools]

-- implies distributes over xor (FALSE)
implies_xor = and [(p <= (q /= r)) == ((p <= q) /= (p <= r)) | p <- bools, q <- bools, r <- bools]


-- Translate each of the statements below, first, in a comment after "A: ",
-- into a logical statement involving forall, exists, /\, \/, ->, and not,
-- and then into Haskell code that checks ("brute force") whether the
-- statement is true. The universe of discourse in each case is u. Your
-- code should work with any universe u (try out several!), but here is
-- a particular one you can use (the order of elements shouldn't matter,
-- since both `and` and `or` are commutative):

u = [13,4,12,22,9,1,2,17,5]

-- I'll work the first example in two different ways

-- 1. "Every number that's greater than 2 is greater than 1"
-- A: forall n, (n > 2) -> (n > 1)
prob1  = and [(n > 2) <= (n > 1) | n <- u]    -- direct solution
prob1' = and [n > 1 | n <- u, n > 2]          -- using a bounded quantifier

-- 2. Every number is either greater than 5 or less than 6
-- A: forall n, (n > 5) \/ (n < 6)
prob2 = and [(n > 5) || (n < 6) | n <- u]

-- 3. Every two numbers are comparable with <= (i.e., either one is <=
--    the other or vice-versa)
-- A: forall x and y, (x <= y) \/ (y <= x)
prob3 = and [(x <= y) || (y <= x) | x <- u, y <- u]

-- 4. For every odd number, there is a greater even number (use the Haskell
--    predicates odd, even :: Integral a => a -> Bool)
-- A: forall x and y, x < y where x is odd and y is even
prob4 = and [(or [x < y | y <- (filter (even) u)]) | x <- (filter (odd) u)]

-- 5. For every even number, there is a greater odd number
-- A: forall x and y, x > y where x is odd and y is even
prob5 = and [(or [x > y | y <- (filter (even) u)]) | x <- (filter (odd) u)]

-- 6. There are two odd numbers that add up to 6
-- A: there exists odd x and y where x + y = 6
prob6 = or [(x + y == 6) | x <- (filter (\n -> odd n) u), y <- (filter (\n -> odd n) u)]

-- 7. There are two even numbers that add up to 20
-- A: there exists even x and y where x + y = 20
prob7 = or [(x + y == 20) | x <- (filter (\n -> even n) u), y <- (filter (\n -> even n) u)]

-- 8. There is a largest number (i.e., there is a number that is >= all numbers)
-- A: there exists x, where x >= y
prob8 = or [(or [x >= y | y <- u]) | x <- u]

-- 9. For every two different numbers, there is a third number in between.
-- A: forall n and x there exists m where (n < m < x) \/ (x < m < n)
prob9 = and [(n < m) && (m < x) || (n > m) && (n > x) | x <- u, n <- u, m <- u]

-- 10. For every number, there is a different number such that there are no
--     numbers between these two.
-- A:  forall n there exists x where n >= m
prob10 = and [(or [(abs (x - n) == 1) | x <- u]) | n <- u]


