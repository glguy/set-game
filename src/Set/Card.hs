module Set.Card
  (-- * Types
   Card(..),
   Color(..),
   Count(..),
   Shading(..),
   Symbol(..),

   -- * 'Card' interface functions
   allCards,
   validSet,
   solve
  ) where

import Data.List                        (tails)

import Set.Utils
  
data Color = Red | Purple | Green
 deriving (Show, Eq)

data Count = One | Two | Three
 deriving (Show, Eq)
 
data Shading = Open | Striped | Solid
 deriving (Show, Eq)
 
data Symbol = Diamond | Squiggle | Oval
 deriving (Show, Eq)

data Card = Card { color :: Color
                 , count :: Count
                 , shading :: Shading
                 , symbol :: Symbol
                 }
 deriving (Show, Eq)

checkAttribute :: Eq a => [a] -> Bool
checkAttribute xs = all (uncurry (==)) combos
                 || all (uncurry (/=)) combos
 where
 combos = chooseTwo xs

validSet :: Card -> Card -> Card -> Bool
validSet card1 card2 card3
  = checkAttribute (map color   cards)
 && checkAttribute (map count   cards)
 && checkAttribute (map shading cards)
 && checkAttribute (map symbol  cards)
  where
  cards = [card1, card2, card3] 

allCards :: [Card]
allCards = [Card a b c d | a <- [Red,     Purple,   Green]
                         , b <- [One,     Two,      Three]
                         , c <- [Open,    Striped,  Solid]
                         , d <- [Diamond, Squiggle, Oval]]


-- | 'solveBoard' returns the list of all valid sets contained in the given
--   list.
solve :: [Card] -> [(Card,Card,Card)]
solve xs = [(a,b,c) | (a:as) <- tails xs
                    , (b:bs) <- tails as
                    , c      <- bs
                    , validSet a b c]
