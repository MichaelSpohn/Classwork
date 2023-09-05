 ---- Lab 11 ----
-- Michael Spohn --
--   110360841   --

---- Relations and some examples

-- For this lab, the universe U is [1..8] and all relations are on this.
u = [1..8]

-- A relation, R, on U is a list of ordered pairs of elements of U:
type Reln = [(Int,Int)]
              
-- As examples, here are the <, <=, =, and /= relations, as well as
-- equivalence mod 3, on u. Take a look at these for inspiration.

lt :: Reln
lt = [(i,j) | j <- u, i <- [1..j-1]]

leq :: Reln
leq = [(i,j) | j <- u, i <- [1..j]]
            
eq :: Reln
eq = [(i,i) | i <- u]

neq :: Reln
neq = [(i,j) | i <- u, j <- u, i /= j]

eqmod3 :: Reln
eqmod3 = [(i,j) | i <- u, j <- u, (j - i) `mod` 3 == 0]


---------------- Lab 11 begins here ----------------

-- Write a function refl that tests whether a relation is reflexive:
-- R is reflexive if: forall a, (a,a) in R (where quantification is over u)
refl :: Reln -> Bool
refl rs = and [elem (a,a) rs | a <- u]

-- Write a function symm that tests whether a relation is symmetric:
-- R is symmetric if: forall a b, (a,b) in R -> (b,a) in R
symm :: Reln -> Bool
symm rs = and [elem (a,b) rs <= elem (b,a) rs | a <- u, b <- u]

-- Write a function trans that tests whether a relation is transitive:
-- R is transistive if: forall a b c, (a,b) in R /\ (b,c) in R -> (a,c) in R
trans :: Reln -> Bool
trans rs = and [(elem (a,b) rs && elem (b,c) rs) <= elem (a,c) rs | a <- u, b <- u, c <- u]

-- Write a function antisymm that tests whether a relation is antisymmetric:
-- R is antisymmetric if: forall a b, (a,b) in R /\ (b,a) in R -> a = b
antisymm :: Reln -> Bool
antisymm rs = and [(elem (a,b) rs && elem (b,a) rs) <= (a == b) | a <- u, b <- u]


-- For each of the 16 possible combinations of yes/no on reflexivity,
-- symmetry, transitivity, and antisymmetry find a MINIMAL relation on u
-- (i.e., one  with the fewest number of pairs) that has exactly that
-- combination of properties, and show the test that confirms the properties.
-- Note: please FIND the relation first, THEN test!! I've done the first one.

-- refl, symm, trans, antisymm
rsta :: Reln
rsta = [(1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8)]
rsta_test = refl rsta && symm rsta && trans rsta && antisymm rsta  -- True

-- refl, symm, trans, not antisymm
rsta' :: Reln
rsta' = [(1,1), (1,2), (2,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8)]
rsta'_test = refl rsta' && symm rsta' && trans rsta' && not (antisymm rsta') -- True

-- refl, symm, not trans, antisymm
-- This combination is impossible! (Why?)
{- In order for this combination to not be transitive, pairs would need to be
   removed in order to remove any possibility of a transitive pattern, however
   it would then remove the possibility of reflexive working. Adding in pairs
   wouldn't work either since that would remove the possibility of antisymmetric
   working. -}

-- refl, symm, not trans, not antisymm
rst'a' :: Reln
rst'a' = [(1,1), (1,2), (2,3), (2,1), (3,2), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8)]
rst'a'_test = refl rst'a' && symm rst'a' && not (trans rst'a') && not (antisymm rst'a') -- True

-- refl, not symm, trans, antisymm
rs'ta :: Reln
rs'ta = [(1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8), (1,2)]
rs'ta_test = refl rs'ta && not (symm rs'ta) && trans rs'ta && antisymm rs'ta -- True

-- refl, not symm, trans, not antisymm
rs'ta' :: Reln
rs'ta' = [(1,1), (1,2), (2,2), (3,3), (3,4), (4,3), (4,4), (5,5), (6,6), (7,7), (8,8)]
rs'ta'_test = refl rs'ta' && not (symm rs'ta') && trans rs'ta' && not (antisymm rs'ta') -- True

-- refl, not symm, not trans, antisymm
rs't'a :: Reln
rs't'a = [(1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8), (1,3), (3,2)]
rs't'a_test = refl rs't'a && not (symm rs't'a) && not (trans rs't'a) && antisymm rs't'a -- True

-- refl, not symm, not trans, not antisymm
rs't'a' :: Reln
rs't'a' = [(1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8), (1,3), (1,4), (3,1)]
rs't'a'_test = refl rs't'a' && not (symm rs't'a') && not (trans rs't'a') && not (antisymm rs't'a') -- True

-- not refl, symm, trans, antisymm
r'sta :: Reln
r'sta = [(1,1)]
r'sta_test = not (refl r'sta) && symm r'sta && trans r'sta && antisymm r'sta -- True

-- not refl, symm, trans, not antisymm
r'sta' :: Reln
r'sta' = [(1,2), (2,1), (1,1), (2,2)]
r'sta'_test = not (refl r'sta') && symm r'sta' && trans r'sta' && not (antisymm r'sta') -- True

-- not refl, symm, not trans, antisymm
-- This combination is impossible! (Why?)
{- In order to have a combination with symmetric and antisymmetric, transitive would
   have to be included or else antisymmetric wouldn't work. Symmetric is easy to
   establish but can't be alone with antisymmetric; it requires transitive. -}

-- not refl, symm, not trans, not antisymm
r'st'a' :: Reln
r'st'a' = [(1,2), (2,1)]
r'st'a'_test = not (refl r'st'a') && symm r'st'a' && not (trans r'st'a') && not (antisymm r'st'a') -- True

-- not refl, not symm, trans, antisymm
r's'ta :: Reln
r's'ta = [(1,2), (2,3), (1,3)]
r's'ta_test = not (refl r's'ta) && not (symm r's'ta) && trans r's'ta && antisymm r's'ta -- True

-- not refl, not symm, trans, not antisymm
r's'ta' :: Reln
r's'ta' = [(1,1), (1,2), (1,3), (2,1), (2,2), (2,3)]
r's'ta'_test = not (refl r's'ta') && not (symm r's'ta') && trans r's'ta' && not (antisymm r's'ta') -- True

-- not refl, not symm, not trans, antisymm
r's't'a :: Reln
r's't'a = [(1,2), (2,3)]
r's't'a_test = not (refl r's't'a) && not (symm r's't'a) && not (trans r's't'a) && antisymm r's't'a -- True

-- not refl, not symm, not trans, not antisymm
r's't'a' :: Reln
r's't'a' = [(1,3), (1,4), (3,1)]
r's't'a'_test = not (refl r's't'a') && not (symm r's't'a') && not (trans r's't'a') && not (antisymm r's't'a') -- True 


