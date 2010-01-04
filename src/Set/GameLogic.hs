{-# LANGUAGE NamedFieldPuns #-}
module Set.GameLogic (
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
        , currentTableau
        , deckNull
        , deckSize
        , hint
  ) where

import Control.Monad (guard)

import Set.Card
import Set.Utils
import Data.List (sortBy)

data Game = Game
  { tableau, deck :: [Card] }

currentTableau :: Game -> [Card]
currentTableau = tableau

-- | 'newGame' creates a new 'Game' with a full tableau and shuffled deck.
newGame :: IO Game
newGame = do
  d <- shuffleIO allCards
  return (deal Game { tableau = [] , deck = d })

-- | 'tableauSize' is the minimum number of cards that should be on the tableau.
tableauSize :: Int
tableauSize = 12

deal :: Game -> Game
deal game = addCards (tableauSize - length (tableau game)) game

shuffleTableau :: Game -> IO Game
shuffleTableau game = do
  tableau' <- shuffleIO (tableau game)
  return game { tableau = tableau' }

considerSet :: Card -> Card -> Card -> Game -> Maybe Game
considerSet card0 card1 card2 game = do
  guard (validSet card0 card1 card2)
  tableau' <- delete1 card0 =<< delete1 card1 =<< delete1 card2 (tableau game)
  return (deal game { tableau = tableau' })

addCards :: Int -> Game -> Game
addCards n Game { tableau, deck } =
  Game { tableau = tableau ++ dealt, deck = deck' }
  where
  (dealt, deck')    = splitAt n deck

deckSize :: Game -> Int
deckSize Game { deck } = length deck

deckNull :: Game -> Bool
deckNull Game { deck } = null deck

extraCards :: Game -> Either Int Game
extraCards game
  | sets == 0 && not (deckNull game) = Right (addCards 3 game)
  | otherwise                        = Left sets
  where
   sets = length (solve (tableau game))

hint :: Game -> IO (Maybe Card)
hint Game { tableau } = do
  tableau' <- shuffleIO tableau
  case solve tableau' of
    ((a,_,_):_) -> return (Just a)
    _ -> return Nothing

sortTableau :: (Card -> Card -> Ordering) -> Game -> Game
sortTableau f game = game { tableau = sortBy f (tableau game) }
