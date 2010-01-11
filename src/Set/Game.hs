{-# LANGUAGE NamedFieldPuns #-}
module Set.Game (
        -- * Types
          Game

        -- * 'Game' creation functions
        , newGame

        -- * 'Game' update functions
        , considerSet
        , extraCards
        , shuffleTableau
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

data Game = Game [Card] [Card]

tableau :: Game -> [Card]
tableau (Game t _) = t

-- | 'newGame' creates a new 'Game' with a full tableau and shuffled deck.
newGame :: IO Game
newGame = (deal . Game []) `fmap` shuffleIO allCards

-- | 'tableauSize' is the minimum number of cards that should be on the tableau.
tableauSize :: Int
tableauSize = 12

deal :: Game -> Game
deal game@(Game t _) = addCards (tableauSize - length t) game

shuffleTableau :: Game -> IO Game
shuffleTableau (Game t d) = do
  t' <- shuffleIO t
  return (Game t' d)

considerSet :: Card -> Card -> Card -> Game -> Maybe Game
considerSet card0 card1 card2 (Game t d) = do
  guard (validSet card0 card1 card2)
  t' <- delete1 card0 =<< delete1 card1 =<< delete1 card2 t
  return (deal (Game t' d))

addCards :: Int -> Game -> Game
addCards n (Game t d) = Game (t ++ dealt) d'
  where
  (dealt, d')    = splitAt n d

deckSize :: Game -> Int
deckSize (Game _ d) = length d

deckNull :: Game -> Bool
deckNull (Game _ d) = null d

extraCards :: Game -> Either Int Game
extraCards game
  | sets == 0 && not (deckNull game) = Right (addCards 3 game)
  | otherwise                        = Left sets
  where
   sets = length (solve (tableau game))

hint :: RandomGen g => g -> Game -> (Maybe Card, g)
hint g game =
  let (tableau', g') = shuffle (tableau game) g
  in case solve tableau' of
      ((a,_,_):_) -> (Just a, g')
      _ -> (Nothing, g')

sortTableau :: (Card -> Card -> Ordering) -> Game -> Game
sortTableau f (Game t d) = Game (sortBy f t) d

-- | 'emptyGame' tests for a game with no cards remaining
--   in either the tableau or the deck.
emptyGame :: Game -> Bool
emptyGame game = null (tableau game) && deckNull game

