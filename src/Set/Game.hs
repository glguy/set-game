{-# LANGUAGE NamedFieldPuns #-}
module Set.Game (
        -- * Types
          Game

        -- * 'Game' creation functions
        , newGame

        -- * 'Game' update functions
        , considerSet
        , extraCards
        , sortTableau

        -- * Game query functions
        , tableau
        , deckNull
        , deckSize
        , emptyGame
        , hint
  ) where

import Control.Monad (guard)
import System.Random (RandomGen)

import Set.Card
import Set.Utils
import Data.List (sortBy)

-- | 'tableauSize' is the minimum number of cards that should be on the tableau.
tableauSize :: Int
tableauSize = 12

-- | 'Game' represents the current state of a Set game including the remaining
-- shuffled deck and the current tableau.
data Game = Game [Card] [Card]

tableau :: Game -> [Card]
tableau (Game t _) = t

-- | 'newGame' creates a new 'Game' with a full tableau and shuffled deck.
newGame :: IO Game
newGame = (deal . Game []) `fmap` shuffleIO allCards

-- | 'deal' adds additional cards as needed to reach 'tableauSize' cards.
deal :: Game -> Game
deal game = addCards (tableauSize - length (tableau game)) game

-- | 'considerSet' verifies that a given set exists in the tableau
-- and then removes the set from the tableau.
considerSet :: Card -> Card -> Card -> Game -> Maybe Game
considerSet card0 card1 card2 (Game t d) = do
  guard (validSet card0 card1 card2)
  t' <- delete1 card0 =<< delete1 card1 =<< delete1 card2 t
  return (deal (Game t' d))

-- | 'addCards' adds the top @n cards from the deck to the end of the tableau.
addCards :: Int -> Game -> Game
addCards n (Game t d) = Game (t ++ dealt) d'
  where
  (dealt, d')    = splitAt n d

-- | 'deckSize' returns the number of cards remaining in the deck.
deckSize :: Game -> Int
deckSize (Game _ d) = length d

-- | 'deckNull' returns 'True' iff the deck is empty.
deckNull :: Game -> Bool
deckNull (Game _ d) = null d

-- | 'extraCards' returns either a new game with 3 additional cards dealt
-- or the number of sets remaining in the tableau.
extraCards :: Game -> Either Int Game
extraCards game
  | sets == 0 && not (deckNull game) = Right (addCards 3 game)
  | otherwise                        = Left sets
  where
   sets = length (solve (tableau game))

-- | 'hint' returns a randomly selected card contained in a set found on
-- the tableau.
hint :: RandomGen g => g -> Game -> (Maybe Card, g)
hint g game =
  let (tableau', g') = shuffle (tableau game) g
  in case solve tableau' of
      ((a,_,_):_) -> (Just a, g')
      _ -> (Nothing, g')

-- | 'sortTableau' sorts the tableau with a given order.
sortTableau :: (Card -> Card -> Ordering) -> Game -> Game
sortTableau f (Game t d) = Game (sortBy f t) d

-- | 'emptyGame' tests for a game with no cards remaining
--   in either the tableau or the deck.
emptyGame :: Game -> Bool
emptyGame game = null (tableau game) && deckNull game

