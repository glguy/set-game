{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Applicative              ((<$),(<$>))
import Control.Monad                    (liftM2, unless)
import Data.Char                        (isDigit, isSpace)
import Data.Ord                         (comparing)
import Data.Monoid                      (mconcat)
import System.Console.Editline.Readline (readline, addHistory)
import Text.ParserCombinators.ReadP     (ReadP, readP_to_S, (+++), munch1,
                                         many, many1, skipSpaces, string, eof)
import qualified System.Console.Terminfo as TI
  
import Set.Card
import Set.GameLogic
import Set.Ascii
import Set.Utils

main :: IO ()
main = do
  term <- TI.setupTermFromEnv
  game <- newGame
  let ?term = term
  run game

data Command = Deal | SelectSet Int Int Int | Hint | Shuffle | Help | Example
             | Sort (Card -> Card -> Ordering)

run :: (?term :: TI.Terminal) => Game -> IO ()
run game | noCards game = putStrLn "Game over!"
run game0 = do
  let (game, dealt) = deal game0
  unless (dealt == 0)
    (putStrLn ("Dealing " ++ show dealt ++ " cards"))

  printGame game

  sel <- prompt "Selection: "
  case sel of
    Nothing                     -> return ()
    Just Deal                   -> checkNoSets game
    Just Hint                   -> giveHint game >> run game
    Just Help                   -> help >> run game
    Just Shuffle                -> run =<< shuffleTableau game
    Just Example                -> example >> run game
    Just (SelectSet a b c)      -> checkSet a b c game
    Just (Sort f)		-> run (sortTableau f game)

printGame :: (?term::TI.Terminal) => Game -> IO ()
printGame Game { tableau, deck } = do
  putStrLn ("Cards remaining deck: " ++ show (length deck))
  putStr (renderTableau tableau)

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
         -> Game 
         -> IO ()
checkSet a b c Game {tableau, deck} = do
  putStrLn message
  run (Game nextTableau deck)
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


checkNoSets :: (?term :: TI.Terminal) => Game -> IO ()
checkNoSets game = case extraCards game of
  Right (_, 0) -> putStrLn "Game over!"
  Right (game', n) -> do
       putStrLn ("Dealing " ++ show n ++ " more cards")
       run game'
  Left sets -> do
       putStrLn $ "Oops, " ++ show sets ++ " sets in tableau. Keep looking."
       run game

-- | 'prompt' wraps 'readline' with a 'Read' parser and repeats the prompt
--   on a failed parse.
prompt :: Read a => String -> IO (Maybe a)
prompt p = parseLn ?=<< readline p
  where
  parseLn ln = do
    addHistory ln
    case reads ln of
      [(x,white)] | all isSpace white -> return (Just x)
      _ -> do putStrLn "Bad input"
              prompt p

instance Read Command where
  readsPrec _ = readP_to_S $ do
                    res <- (Hint    <$ string "hint")
                       +++ (Deal    <$ string "deal")
                       +++ (Shuffle <$ string "shuffle")
                       +++ (Help    <$ string "help")
                       +++ (Example <$ string "example")
                       +++ sortP
                       +++ selectSetP
                    skipSpaces
                    eof 
                    return res
    where
    selectSetP = do
      [a,b,c] <- many (skipSpaces >> intP)
      return (SelectSet a b c)

    intP = read <$> munch1 isDigit

    sortP = do
      _ <- string "sort"
      fmap (Sort . mconcat) (many1 (whiteSpace >> ordP))

    ordP = (comparing color   <$ string "color")
       +++ (comparing shading <$ string "shading")
       +++ (comparing count   <$ string "count")
       +++ (comparing symbol  <$ string "symbol")

whiteSpace :: ReadP ()
whiteSpace = munch1 isSpace >> return ()
