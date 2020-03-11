--HW 1 By: Megan Liles
-- ONID: lilesme

module HW1 where

-- | Integer-labeled binary trees.
data Tree
   = Node Int Tree Tree   -- ^ Internal nodes
   | Leaf Int             -- ^ Leaf nodes
  deriving (Eq,Show)

-- | An example binary tree, which will be used in tests.
t1 :: Tree
t1 = Node 1 (Node 2 (Node 3 (Leaf 4) (Leaf 5))
                    (Leaf 6))
            (Node 7 (Leaf 8) (Leaf 9))

-- | Another example binary tree, used in tests.
t2 :: Tree
t2 = Node 6 (Node 2 (Leaf 1) (Node 4 (Leaf 3) (Leaf 5)))
            (Node 8 (Leaf 7) (Leaf 9))

-- | The integer at the left-most node of a binary tree.
--
--   >>> leftmost (Leaf 3)
--   3
--
--   >>> leftmost (Node 5 (Leaf 6) (Leaf 7))
--   6
--
--   >>> leftmost t1
--   4
--
--   >>> leftmost t2
--   1

--leftmost = undefined
leftmost :: Tree -> Int
leftmost (Leaf v) = v --return value if only one value
leftmost (Node _ y _) = leftmost y --traverse down leftside of tree

-- | The integer at the right-most node of a binary tree.
--
--   >>> rightmost (Leaf 3)
--   3
--
--   >>> rightmost (Node 5 (Leaf 6) (Leaf 7))
--   7
--
--   >>> rightmost t1
--   9
--
--   >>> rightmost t2
--   9

--rightmost = undefined
rightmost :: Tree -> Int
rightmost (Leaf v) = v --return value if only one value
rightmost (Node _ _ y) = rightmost y --traverse down right side of tree


-- | Get the maximum integer from a binary tree.
--
--   >>> maxInt (Leaf 3)
--   3
--
--   >>> maxInt (Node 5 (Leaf 4) (Leaf 2))
--   5
--
--   >>> maxInt (Node 5 (Leaf 7) (Leaf 2))
--   7
--
--   >>> maxInt t1
--   9
--
--   >>> maxInt t2
--   9
--
maxInt :: Tree -> Int
maxInt (Leaf v) = v --return value if only one value
maxInt (Node v l r) = max v (max (maxInt l)(maxInt r))


-- call isBST, if true than max int is rightmost so call right leftmost
-- if false traverse the tree and keep track of max integer

-- | Get the minimum integer from a binary tree.
--
--   >>> minInt (Leaf 3)
--   3
--
--   >>> minInt (Node 2 (Leaf 5) (Leaf 4))
--   2
--
--   >>> minInt (Node 5 (Leaf 4) (Leaf 7))
--   4
--
--   >>> minInt t1
--   1
--
--   >>> minInt t2
--   1
--
minInt :: Tree -> Int
minInt (Leaf v) = v --return value if only one
minInt (Node v l r) = min v (min (minInt l)(minInt r))


-- | Get the sum of the integers in a binary tree.
--
--   >>> sumInts (Leaf 3)
--   3
--
--   >>> sumInts (Node 2 (Leaf 5) (Leaf 4))
--   11
--
--   >>> sumInts t1
--   45
--
--   >>> sumInts (Node 10 t1 t2)
--   100
--

sumInts :: Tree -> Int
sumInts (Leaf v) = v -- return value if only one value
sumInts (Node v l r) = v + (sumInts l) + (sumInts r) --add value + sum of values on left + sum of value son right



-- | The list of integers encountered by a pre-order traversal of the tree.
--
--   >>> preorder (Leaf 3)
--   [3]
--
--   >>> preorder (Node 5 (Leaf 6) (Leaf 7))
--   [5,6,7]
--
--   >>> preorder t1
--   [1,2,3,4,5,6,7,8,9]
--
--   >>> preorder t2
--   [6,2,1,4,3,5,8,7,9]
--
--First Visit of Nodes
preorder :: Tree  -> [Int]
preorder (Leaf v) = [v] --if only one value return value
preorder (Node v l r) = v : preorder l ++ preorder r -- do root, left, right

-- | The list of integers encountered by an in-order traversal of the tree.
--
--   >>> inorder (Leaf 3)
--   [3]
--
--   >>> inorder (Node 5 (Leaf 6) (Leaf 7))
--   [6,5,7]
--
--   >>> inorder t1
--   [4,3,5,2,6,1,8,7,9]
--
--   >>> inorder t2
--   [1,2,3,4,5,6,7,8,9]
--
-- Second Visit of Nodes
inorder :: Tree  -> [Int]
inorder (Leaf v) = [v] --if only one value return value
inorder (Node v l r) = inorder l ++ (v : inorder r) --  do left, root, right

-- | Check whether a binary tree is a binary search tree.
--
--   >>> isBST (Leaf 3)
--   True
--
--   >>> isBST (Node 5 (Leaf 6) (Leaf 7))
--   False
--
--   >>> isBST t1
--   False
--
--   >>> isBST t2
--   True
--
isBST :: Tree -> Bool
isBST (Leaf v) = True -- if only one value, return true
isBST (Node v l r) = ((maxInt l) < v) -- the biggest int on the left tree is smaller than value
                  && ((minInt r) > v) -- the the smallest int on the right tree is bigger than value
                  && (isBST l) -- repeat with left tree l
                  && (isBST r) -- repeat with right tree r

-- | Check whether a number is contained in a binary search tree.
--   (You may assume that the given tree is a binary search tree.)
--
--   >>> inBST 2 (Node 5 (Leaf 2) (Leaf 7))
--   True
--
--   >>> inBST 3 (Node 5 (Leaf 2) (Leaf 7))
--   False
--
--   >>> inBST 4 t2
--   True
--
--   >>> inBST 10 t2
--   False
--
inBST :: Int -> Tree -> Bool
inBST x (Leaf v) = if x == v then True else False -- if x is value, return true, else return false
inBST x (Node v l r)
	| x == v = True
	| x < v = inBST x l -- if x is less than value, search left tree
	| x > v = inBST x r -- if x is greater than value, search right tree
