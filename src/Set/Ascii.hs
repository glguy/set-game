{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Set.Ascii (renderTableau, renderCardRow, renderCard) where

import Data.List                        (transpose)
import qualified System.Console.Terminfo as TI

import Set.Card
import Set.Utils

-- | 'tableauWidth' is the number of cards to be rendered in each row on the
--   tableau.
tableauWidth :: Int
tableauWidth = 3

-------------------------------------------------------------------------------
-- Card drawing functions -----------------------------------------------------
-------------------------------------------------------------------------------

-- | 'renderTableau' renders a list of 'Card's in a grid with 1-based indexing
--   as a 'String' using the current 'Terminal'.
renderTableau :: (?term :: TI.Terminal) => [Card] -> String
renderTableau = unlines
              . map concatGroup
              . groups tableauWidth
              . zipWith3 addIndex pads [1 :: Int ..]
              . map renderCard
  where
  pads = replicate 9 " " ++ repeat ""
  addIndex pad index = zipWith (++) ((pad ++ show index) : repeat "  ")

-- | 'renderCardRow' renders a list of 'Card's in a row without indexing.
renderCardRow :: (?term :: TI.Terminal) => [Card] -> String
renderCardRow = concatGroup . map padLines . map renderCard
  where padLines = map ("  "++)

concatGroup :: [[String]] -> String
concatGroup = unlines . map concat . transpose

-- | 'renderCard' renders a 'Card' to a list of lines with the appropriate art
--   and coloring and duplication.
renderCard :: (?term :: TI.Terminal) => Card -> [String]
renderCard Card {color, count, shading, symbol}
  = map (addColor color . duplicate count) (selectArt shading symbol)

-- | 'duplicate' pads a 'String' to fit neatly, centered in a 14-character
--   region.
duplicate :: Count -> String -> String
duplicate One   x = "      " ++        x        ++ "      "
duplicate Two   x = "   "    ++ x ++ "  " ++ x  ++    "   "
duplicate Three x = " " ++ x ++ " " ++ x ++ " " ++ x ++ " "

addColor :: (?term :: TI.Terminal) => Color -> String -> String
addColor color = f2 TI.Black . f1 termcolor
  where
  termcolor = case color of
    Green  -> TI.Green
    Purple -> TI.Cyan
    Red    -> TI.Red

  Just f1 = TI.getCapability ?term TI.withForegroundColor
  Just f2 = TI.getCapability ?term TI.withBackgroundColor

-- | 'selectArt' returns the ASCII art representation of the lines for
--   a given 'Shading' and 'Symbol'.
selectArt :: Shading -> Symbol -> [String]
selectArt Open    Diamond  = ["    "
                             ," ╱╲ "
                             ,"╱  ╲"
                             ,"‾‾‾‾"]
selectArt Striped Diamond  = ["    "
                             ," ╱╲ "
                             ,"╱╱╲╲"
                             ,"‾‾‾‾"]
selectArt Solid   Diamond  = ["    "
                             ," ╱╲ "
                             ,"╱╳╳╲"
                             ,"‾‾‾‾"]
selectArt Open    Squiggle = ["___ "
                             ,"╲  ╲"
                             ,"╱  ╱"
                             ,"‾‾‾ "]
selectArt Striped Squiggle = ["___ "
                             ,"╲╲╲╲"
                             ,"╱╱╱╱"
                             ,"‾‾‾ "]
selectArt Solid   Squiggle = ["___ "
                             ,"╲╳╳╲"
                             ,"╱╳╳╱"
                             ,"‾‾‾ "]
selectArt Open    Oval     = [" __ "
                             ,"╱  ╲"
                             ,"╲  ╱"
                             ," ‾‾ "]
selectArt Striped Oval     = [" __ "
                             ,"╱╱╲╲"
                             ,"╲╲╱╱"
                             ," ‾‾ "]
selectArt Solid   Oval     = [" __ "
                             ,"╱╳╳╲"
                             ,"╲╳╳╱"
                             ," ‾‾ "]
