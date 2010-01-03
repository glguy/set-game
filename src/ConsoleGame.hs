{-# LANGUAGE ImplicitParams #-}
module Main where

import Control.Applicative              ((<$))
import Control.Monad                    (liftM2, unless)
import Data.Char                        (isDigit, isSpace)
import System.Console.Editline.Readline (readline, addHistory)
import Text.ParserCombinators.ReadP     (readP_to_S, (+++), munch1,
                                         sepBy, skipSpaces, string)
import qualified System.Console.Terminfo as TI
  
import Set.Card
import Set.Ascii
import Set.Utils

main :: IO ()
main = do
  term <- TI.setupTermFromEnv
  let ?term = term
  game

-- | 'tableauSize' is the minimum number of cards that should be on the tableau.
tableauSize :: Int
tableauSize = 12


data Command = Deal | SelectSet Int Int Int | Hint | Shuffle | Help | Example
  deriving (Eq, Show)

game :: (?term :: TI.Terminal) => IO ()
game = game' [] =<< shuffleIO allCards

game' :: (?term :: TI.Terminal) => [Card] -> [Card] -> IO ()
game' [] [] = putStrLn "Game over!"
game' tableau deck = do
  (tableau', deck') <- deal tableau deck

  putStrLn ("Cards remaining deck: " ++ show (length deck'))
  putStr (renderTableau tableau')

  sel <- prompt "Selection: "
  case sel of
    Nothing                     -> return ()
    Just Deal                   -> checkNoSets tableau' deck'
    Just Hint                   -> hint tableau' >> game' tableau' deck'
    Just Help                   -> help >> game' tableau' deck'
    Just Shuffle                -> flip game' deck' =<< shuffleIO tableau'
    Just Example                -> example >> game' tableau' deck'
    Just (SelectSet a b c)      -> checkSet a b c tableau' deck'

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

-- | 'deal' adds cards to the tableau from the deck up to a minimum of
--   'tableauSize' cards.
deal :: [Card] -> [Card] -> IO ([Card],[Card])
deal tableau deck = do
  unless (null dealt) (putStrLn ("Dealing " ++ show (length dealt) ++ " cards"))
  return (tableau ++ dealt, deck')
 where
  (dealt, deck')    = splitAt (tableauSize - length tableau) deck

hint :: (?term::TI.Terminal) => [Card] -> IO ()
hint tableau = do
  tableau' <- shuffleIO tableau
  case solve tableau' of
    ((a,_,_):_) -> do putStrLn "There is a set using this card."
                      putStrLn $ unlines $ renderCard a

    _           -> do putStrLn "No solutions"

-- | 'checkSet' will extract the chosen set from the tableau and check it
--   for validity. If a valid set is removed from the tableau the tableau
--   will be refilled up to 12 cards.
checkSet :: (?term :: TI.Terminal)
         => Int    -- ^ Index of first card in set
         -> Int    -- ^ Index of second card in set
         -> Int    -- ^ Index of third card in set
         -> [Card] -- ^ Current tableau
         -> [Card] -- ^ Current deck
         -> IO ()
checkSet a b c tableau deck = do
  putStrLn message
  game' nextTableau deck
  where
  (card0, card1, card2, tableau') = select3 (a-1) (b-1) (c-1) tableau

  row = renderCardRow [card0, card1, card2]

  (message, nextTableau)
    | not validInput                    = ("Invalid selection.\n", tableau)
    | validSet card0 card1 card2        = (row ++ "Good job.\n", tableau')
    | otherwise                         = (row ++ "Not a set!\n", tableau)

  inputs = [a,b,c]
  n = length tableau
  validInput = all (uncurry (/=)) (chooseTwo inputs)
            && all (bounded 1 n) inputs


checkNoSets :: (?term :: TI.Terminal) => [Card] -> [Card] -> IO ()
checkNoSets tableau deck
  | sets == 0 && null deck = do
       putStrLn "Game over!"

  | sets == 0 = do
       putStrLn "Dealing three more cards"
       let (replacement, deck') = splitAt 3 deck
       game' (tableau ++ replacement) deck'
    
  | otherwise = do
       putStrLn $ "Oops, " ++ show sets ++ " sets in tableau. Keep looking."
       game' tableau deck
  where
       sets = length (solve tableau)

-- | 'prompt' wraps 'readline' with a 'Read' parser and repeats the prompt
--   on a failed parse.
prompt :: Read a => String -> IO (Maybe a)
prompt p = parseLn ?=<< readline p
  where
  parseLn ln = case reads ln of
    [(x,white)] | all isSpace white -> do addHistory ln
                                          return (Just x)
    _ -> do putStrLn "Bad input"
            prompt p

instance Read Command where
  readsPrec _ = readP_to_S $ (Hint    <$ string "hint")
                         +++ (Deal    <$ string "deal")
                         +++ (Shuffle <$ string "shuffle")
                         +++ (Help    <$ string "help")
                         +++ (Example <$ string "example")
                         +++ selectSetP
    where
    selectSetP = do
      [a,b,c] <- intP `sepBy` skipSpaces
      return (SelectSet a b c)

    intP = read `fmap` munch1 isDigit
