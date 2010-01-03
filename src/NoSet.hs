module Main where

import Set
import Set.Utils
import Set.Ascii

search :: [Card] -> [Card]
search = foldl considerCard []
 where
 considerCard sol x
   | any (uncurry (validSet x)) (chooseTwo sol) = sol
   | otherwise = x : sol
