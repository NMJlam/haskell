module Main (main) where

import BinTree

main :: IO ()
main = do
  let oneDepth = depth one
  let treeDepth = depth tree

  putStrLn ("The depth of 'one' is " ++ show oneDepth)
  putStrLn ("The depth of 'tree' is " ++ show treeDepth)


  let modTree = mapTree (`mod` 2) tree
  print modTree 

