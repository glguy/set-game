module Main where

import Data.Monoid (mempty)
import SetGame (gameMain)

main :: IO ()
main = gameMain mempty
