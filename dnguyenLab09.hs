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
