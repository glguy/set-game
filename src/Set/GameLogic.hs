{-# LANGUAGE NamedFieldPuns #-}
module Set.GameLogic where

import Set.Card
import Set.Utils
import Data.List (sortBy)

data Game = Game
  { tableau, deck :: [Card] }

newGame :: IO Game
newGame = do
  d <- shuffleIO allCards
  return Game { tableau = [] , deck = d }

-- | 'tableauSize' is the minimum number of cards that should be on the tableau.
tableauSize :: Int
tableauSize = 12

deal :: Game -> (Game, Int)
deal game = addCards (tableauSize - length (tableau game)) game

shuffleTableau :: Game -> IO Game
shuffleTableau game = do
  tableau' <- shuffleIO (tableau game)
  return game { tableau = tableau' }

addCards :: Int -> Game -> (Game, Int)
addCards n Game { tableau, deck } = (game', length dealt)
  where
  game' = Game { tableau = tableau ++ dealt,
                 deck    = deck' }
  (dealt, deck')    = splitAt n deck

noCards :: Game -> Bool
noCards Game { tableau = [], deck = [] } = True
noCards _ = False

extraCards :: Game -> Either Int (Game, Int)
extraCards game
  | sets == 0 = Right (addCards 3 game)
  | otherwise = Left sets
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
