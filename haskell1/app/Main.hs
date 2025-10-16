module Main (main) where

import BinTree
import NaryTree
import MappingMaybe 

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
