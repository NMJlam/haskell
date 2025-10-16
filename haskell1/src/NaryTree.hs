-- | Implementation of integer valued binary trees.
--
-- The focus of these exercises is to get comfortable with a more
-- complex custom type, and using recursion.
module NaryTree
  ( NaryTree (Leaf, Node),
    naryTree,
    depth,
    mapTree,
  )
where

-- | A NaryTree is a type of tree which has a list of children
--
-- The children can be either
-- - a Leaf with no value, or
-- - another NaryTree
--
-- See https://tgdwyer.github.io/haskell2/#algebraic-data-types
data NaryTree a = Leaf | Node a [NaryTree a]
  deriving (Show)

-- Example Trees
naryTree :: NaryTree Int
naryTree = Node 2 [Node 3 [Node 41 [], Node 42 [], Node 43 []], Node 5 [Node 6 []]]

-- | Find the depth of a tree (number of levels)
--
-- See https://tgdwyer.github.io/haskell2/#pattern-matching
--
-- >>> depth Leaf
-- 0
--
-- >>> depth (Node 1 [])
-- 1
--
-- >>> depth tree
-- 3
depth :: NaryTree a -> Int
depth Leaf = 0
depth (Node _ children) =
  if null children
    then 1
    else 1 + maximum (map depth children)

-- | Map a function over a tree.
--
-- >>> mapTree (+1) Leaf
-- Leaf
--
-- >>> mapTree (*1) (Node 1 []) Node 1 []
--
-- >>> mapTree (`mod` 2) tree
-- Node 0 [Node 1 [Node 1 [],Node 0 [],Node 1 []],Node 1 [Node 0 []]]
mapTree :: (a -> b) -> NaryTree a -> NaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node val children) = Node newVal newChildren
  where
    newChildren = map (mapTree f) children
    newVal = f val
