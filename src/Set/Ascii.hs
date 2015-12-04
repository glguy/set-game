{-# LANGUAGE NamedFieldPuns #-}
module Set.Ascii (cardLines) where

import Set.Card

-------------------------------------------------------------------------------
-- Card drawing functions -----------------------------------------------------
-------------------------------------------------------------------------------

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

-- | Compute the ASCII lines and color of a given card
cardLines :: Card -> (Color, [String])
cardLines Card {color, count, shading, symbol}
  = (color, map (duplicate count) (selectArt shading symbol))

-- | 'duplicate' pads a 'String' to fit neatly, centered in a 14-character
--   region.
duplicate :: Count -> String -> String
duplicate One   x = "      " ++        x        ++ "      "
duplicate Two   x = "   "    ++ x ++ "  " ++ x  ++    "   "
duplicate Three x = " " ++ x ++ " " ++ x ++ " " ++ x ++ " "
