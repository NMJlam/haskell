module Main (main) where

import BinTree
import NaryTree
import MappingMaybe 
import Examples

main :: IO ()
main = do
  let oneDepth = BinTree.depth one
  let treeDepth = BinTree.depth tree

  putStrLn ("The depth of 'one' is " ++ show oneDepth)
  putStrLn ("The depth of 'tree' is " ++ show treeDepth)


  let modTree = BinTree.mapTree (`mod` 2) tree
  print modTree 

  let depthNary = NaryTree.depth naryTree
  putStrLn ("The depth of the NaryTree is " ++ show depthNary)

  let modTreeN = NaryTree.mapTree(`mod` 2) naryTree 
  print modTreeN 

  print (mapMaybe reverse (Just "hi") ) 
  print (flatMapMaybe parseNumber (Just "123"))

  print (chainFunctions "0")
  print (chainFunctions "5")
  print (chainFunctions "foo")

  print (eulerProblem1 1000)

  print (allEvens [0,0,0,4])

  print (anyOdd [1,2,3,4,5])


  print (sumTwoLists [1,2,3,4,5] [1,2,3,4,5])
  
  print (firstItems [(2,1), (4,3), (6,5)])

  print (nestedMap (+1) [[1,2,3], [4,5,6], [7,8,9]])


