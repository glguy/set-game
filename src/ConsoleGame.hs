{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Applicative              ((<$),(<$>), Applicative(..),(<*))
import Control.Monad                    (ap, liftM2)
import Data.Char                        (isSpace)
import Data.Ord                         (comparing)
import Data.Monoid                      (mconcat)
import System.Console.Editline.Readline (readline, addHistory)
import Text.ParserCombinators.ReadP
        (ReadP, readP_to_S, readS_to_P, choice, munch1, skipSpaces,
         string, eof, sepBy1)
import qualified System.Console.Terminfo as TI
  
import Set.Ascii
import Set.Card
import Set.GameLogic
import Set.Utils

main :: IO ()
main = do
  term <- TI.setupTermFromEnv
  game <- newGame
  let ?term = term
  run game

data Command = Deal | SelectSet Int Int Int | Hint | Shuffle | Help | Example
             | Sort (Card -> Card -> Ordering)

emptyGame :: Game -> Bool
emptyGame game = null (currentTableau game) && deckNull game

run :: (?term :: TI.Terminal) => Game -> IO ()
run game | emptyGame game = putStrLn "No card left; Game over!"
run game = do
  let tableauCards = currentTableau game
  printGame tableauCards (deckSize game)

  sel <- prompt "Selection: "
  case sel of
    Nothing                     -> return ()
    Just Deal                   -> checkNoSets game
    Just Hint                   -> giveHint game >> run game
    Just Help                   -> help >> run game
    Just Shuffle                -> run =<< shuffleTableau game
    Just Example                -> example >> run game
    Just (SelectSet a b c)      -> checkSet a b c tableauCards game
    Just (Sort f)		-> run (sortTableau f game)

printGame :: (?term::TI.Terminal) => [Card] -> Int -> IO ()
printGame cards n = do
  putStrLn ("Cards remaining in deck: " ++ show n)
  putStr (renderTableau cards)

example :: (?term :: TI.Terminal) => IO ()
example = do
  let xs = liftM2 (,) [Red,Purple,Green] [One,Two,Three]
      ys = liftM2 (,) [Diamond, Squiggle, Oval] [Solid, Striped, Open]
      cards = zipWith (\ (col,cou) (shap, shad) -> Card col cou shad shap) xs ys
  putStr $ renderTableau cards
  putStrLn "Press enter to continue."
  _ <- getLine
  return ()

help :: (?term :: TI.Terminal) => IO ()
help = do
  putStrLn "# # #     Choose a set"
  putStrLn "deal      Check for sets and deal three more cards if none"
  putStrLn "example   Show example cards of every characteristic"
  putStrLn "help      This menu"
  putStrLn "hint      Show one of the cards that fits in a set"
  putStrLn "shuffle   Shuffle the current tableau"
  putStrLn ""
  putStrLn "Press enter to continue."
  _ <- getLine
  return ()


giveHint :: (?term::TI.Terminal) => Game -> IO ()
giveHint game = do
  mbHint <- hint game
  case mbHint of
    Just a      -> do putStrLn "There is a set using this card."
                      putStrLn $ unlines $ renderCard a

    _           -> do putStrLn "No solutions"

-- | 'checkSet' will extract the chosen set from the tableau and check it
--   for validity. If a valid set is removed from the tableau the tableau
--   will be refilled up to 12 cards.
checkSet :: (?term :: TI.Terminal)
         => Int    -- ^ Index of first card in set
         -> Int    -- ^ Index of second card in set
         -> Int    -- ^ Index of third card in set
         -> [Card]
         -> Game 
         -> IO ()
checkSet a b c cards game
  | uniques [a,b,c] = case mbCards of
      Just (card0, card1, card2) ->
         let row = renderCardRow [card0, card1, card2]
         in case considerSet card0 card1 card2 game of
             Nothing    -> putStrLn (row ++ "Not a set.") >> run game
             Just game' -> putStrLn (row ++ "Good job.")  >> run game'
         
      Nothing -> putStrLn "Invalid selection."  >> run game
  | otherwise = putStrLn "Duplicate selection."  >> run game
  where
  mbCards = (,,) <$> index (a-1) cards
                 <*> index (b-1) cards
                 <*> index (c-1) cards

checkNoSets :: (?term :: TI.Terminal) => Game -> IO ()
checkNoSets game = case extraCards game of
  Right game' -> run game'
  Left 0 -> putStrLn "Game over!"
  Left sets -> do
       putStrLn $ "Oops, " ++ show sets ++ " sets in tableau. Keep looking."
       run game

-- | 'prompt' wraps 'readline' with a 'Read' parser and repeats the prompt
--   on a failed parse.
prompt :: Read a => String -> IO (Maybe a)
prompt p = parseLn =<< readline p
  where
  parseLn Nothing = return Nothing
  parseLn (Just ln) = do
    addHistory ln
    case reads ln of
      [(x,_)] -> return (Just x)
      _ -> do putStrLn "Bad input"
              prompt p

instance Read Command where
  readsPrec _ = readP_to_S commandP

commandP :: ReadP Command
commandP = skipSpaces
        *> choice [Hint      <$  string "hint"
                  ,Deal      <$  string "deal"
                  ,Shuffle   <$  string "shuffle"
                  ,Help      <$  string "help"
                  ,Example   <$  string "example"
                  ,Sort      <$> sortP
                  ,selectSetP]
        <* skipSpaces
        <* eof

selectSetP :: ReadP Command
selectSetP = SelectSet <$> intP <*> (skipSpaces *> intP)
                                <*> (skipSpaces *> intP)

intP :: ReadP Int
intP = readS_to_P reads

sortP :: ReadP (Card -> Card -> Ordering)
sortP = string "sort" *> skipSpaces1
     *> (mconcat <$> sepBy1 ordP skipSpaces1)
  where
    ordP = choice [comparing color   <$ string "color"
                  ,comparing count   <$ string "count"
                  ,comparing shading <$ string "shading"
                  ,comparing symbol  <$ string "symbol"]

skipSpaces1 :: ReadP ()
skipSpaces1 = () <$ munch1 isSpace

instance Applicative ReadP where
 (<*>) = ap
 pure = return
