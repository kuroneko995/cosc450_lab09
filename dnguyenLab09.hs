-- Dang Minh Nguyen
-- COSC 304
-- Lab 08
-- 11/11/2014

-- Question 1
u b0 f [] = b0
u b0 f (a:rest) = f (a, u b0 f rest)

-- listlength
f1 (a, x) = 1 + x

listlength = u 0 f1

-- listsum
f2 (a, x) = a + x

listsum = u 0 f2

-- monext
monext f bin e = let f3 (a,x) = bin (f a) x
                 in   u e f3
                    
-- flatten
f4 (a, x) = a ++ x

flatten = u [] f4

{-
*Main> listlength [1,3]
2
*Main> listlength [1..5]
5
*Main> listsum [1..5]
15
*Main> monext (*2) (*) 1 [1,3,4]
96
*Main> flatten [[1..3],[2],[3..4]]
[1,2,3,2,3,4]

-}

-- Question 2
data BT a = E1 | BT (BT a, a, BT a) deriving Show
instance Functor BT where
    fmap f E1 = E1
    fmap f (BT (t1, a, t2)) = BT (fmap f t1, f a, fmap f t2)
    
tree1 = BT (BT (E1, 2, E1), 3, BT (E1, 4, E1))
tree2 = BT (BT (E1, [2..4], E1), [3..5], BT (E1, [4..200], E1))

u2 b0 f E1 = b0
u2 b0 f (BT (t1, a, t2)) = f (u2 b0 f t1, a, u2 b0 f t2)  

fbt1 (x, a, y) = x + a + y
btreeSum = u2 0 fbt1

fbt2 (x, a, y) = x + 1 + y
btreeSize = u2 0 fbt2
{-
a/
*Main> fmap (2*) tree1
BT (BT (E1,4,E1),6,BT (E1,8,E1))
*Main> fmap head tree2
BT (BT (E1,2,E1),3,BT (E1,4,E1))

b/
The H algebra looks like this...
H X = 1 + X . a . X

c/
Type of u2 (avoid classing with Q1) is
u2 :: b -> (b -> a -> b -> b) -> BT a -> b

d/ 
*Main> btreeSum tree1
9

e/ Here I assume E1 count as 0 node
*Main> btreeSize tree1
3
-}


-- Question 3
data T a = L a | B (T a) (T a) deriving Show

instance Functor T where
    fmap f (L a)        = L (f a)
    fmap f (B t1 t2)    = B (fmap f t1) (fmap f t2)
    
u3 f0 f (L a)        = f0 a
u3 f0 f (B t1 t2)    = f (u3 f0 f t1, u3 f0 f t2)

btreeSum2   = u3 (\s -> s) (\(a,b) -> a + b)
treeSize2    = u3 (\s -> 1) (\(a,b) -> a + b)

tree3 = B (L 2) (L 3)
tree4 = B (B (L [2..3]) (L [5..10])) (L [4..200])


{-
a/ Testing
*Main> fmap (2*) tree3
B (L 4) (L 6)
*Main> fmap head tree4
B (B (L 2) (L 5)) (L 4)
b/ The H algebra is
H X = A + X . X

c/ type
u :: (a -> b) -> ((b,b) -> b) -> (T a) -> b 

d/*Main> btreeSum2 tree3
5
*Main> treeSize2 tree3
2
*Main> treeSize2 tree4
3

-}


-- Question 4
data Nat = Z | S Nat deriving Show

u4 b0 f Z        = b0
u4 b0 f (S n)    = f (u4 b0 f n)

add x = u4 x (\s -> S s)
mult x = u4 Z (\s -> add x s)

facth = u4 (Z, S Z) (\(n, fn) -> (S n, mult (S n) fn))
fact = snd.facth

{- Testing
*Main> add (S (S Z)) (S Z)
S (S (S Z))
*Main> mult (S (S Z)) (S Z)
S (S Z)
*Main> mult (S (S Z)) (S (S Z))
S (S (S (S Z)))
*Main> facth (S (S (S Z)))
(S (S (S Z)),S (S (S (S (S (S Z))))))
*Main> fact (S (S (S Z)))
S (S (S (S (S (S Z)))))
 
-}