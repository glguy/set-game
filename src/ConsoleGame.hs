module Main where

import Control.Concurrent               (threadDelay)
import Data.List                        (delete)
import Graphics.Vty as Vty
import System.Random (newStdGen, StdGen)

import Set.Ascii
import Set.Card (Card, Color(Red,Purple,Green))
import Set.Game
import Set.Utils hiding (select)

main :: IO ()
main = do
  vty <- mkVty
  game <- newGame
  g <- newStdGen
  run vty game
        $ setGame game
        $ newInterface g
  shutdown vty

run :: Vty -> Game -> Interface -> IO ()
run _ game _ | emptyGame game = return ()
run vty game s = do
  s' <- printGame vty s

  cmd <- handleInput vty
  let simple f = Just (game, f s')
  let res = case cmd of
        Deal        -> checkNoSets game s
        DeleteLast  -> simple $ initSelection
                              . clearMessage
        Hint        -> simple $ giveHint game
        Move dir    -> simple $ updateControl $ moveCur dir $ length
                              $ tableau game
        Quit        -> Nothing
        Select      -> Just (select game s')
  case res of
    Nothing -> return ()
    Just (game', s'') -> run vty game' s''

select :: Game -> Interface -> (Game, Interface)
select game s = case iControl s of
 CardButton i ->
  case index i (tableau game) of
   Just t | isSelected t s -> (game, updateSelection (delete t)
                                   . clearMessage
                                   $ s)
          | otherwise    -> addCard t game s
   Nothing               -> (game, setControl (CardButton 0) s)

addCard :: Card -> Game -> Interface -> (Game, Interface)
addCard card0 game s = case iSelection s of
  (_:_:_:_)      -> (game, setMessage "Selection full" s)
  [card1, card2] -> checkSet card0 card1 card2 game . appendSelection card0
                                                    . clearMessage
                                                    $ s
  _              -> (game, appendSelection card0
                         . clearMessage
                         $ s)

setGame :: Game -> Interface -> Interface
setGame game = setTableau (tableau game)
             . setRemaining (deckSize game)

-- Interface manipulation functions

data Interface = IS
  { iControl :: CurrentControl
  , iSelection :: [Card]
  , iMessage :: String
  , iDealCounter :: Int
  , iBadDealCounter :: Int
  , iStdGen :: StdGen
  , iTimer :: Maybe (Int, Interface)
  , iTableau :: [Card]
  , iRemaining :: Int
  }

newInterface :: StdGen -> Interface
newInterface g = IS
  { iControl = CardButton 0
  , iSelection = []
  , iMessage = ""
  , iDealCounter = 0
  , iBadDealCounter = 0
  , iStdGen = g
  , iTimer = Nothing
  , iTableau = []
  , iRemaining = 0
  }

data CurrentControl = CardButton Int
 deriving (Eq)

setRemaining :: Int -> Interface -> Interface
setRemaining n s = s { iRemaining = n }

setTableau :: [Card] -> Interface -> Interface
setTableau xs s = s { iTableau = xs }

setTimer :: Int -> Interface -> Interface -> Interface
setTimer delay s' s = s { iTimer = Just (delay, s') }

tempUpdate :: Int -> (Interface -> Interface)
                  -> Interface -> Interface
tempUpdate delay f s = setTimer delay s (f s)

delayedUpdate :: Int -> (Interface -> Interface)
              -> Interface -> Interface
delayedUpdate delay f s = setTimer delay (f s) s

incDealCounter :: Interface -> Interface
incDealCounter i = i { iDealCounter = iDealCounter i + 1 }

incBadDealCounter :: Interface -> Interface
incBadDealCounter i = i { iBadDealCounter = iBadDealCounter i + 1 }

setControl :: CurrentControl -> Interface -> Interface
setControl x i = i { iControl = x }

updateControl :: (CurrentControl -> CurrentControl)
              -> Interface -> Interface
updateControl f i = setControl (f (iControl i)) i

clearMessage :: Interface -> Interface
clearMessage = setMessage ""

setMessage :: String -> Interface -> Interface
setMessage msg i = i { iMessage = msg }

setSelection :: [Card] -> Interface -> Interface
setSelection xs i = i { iSelection = xs }

updateSelection :: ([Card] -> [Card]) -> Interface -> Interface
updateSelection f i = setSelection (f (iSelection i)) i

appendSelection :: Card -> Interface -> Interface
appendSelection card = updateSelection (++ [card])

initSelection :: Interface -> Interface
initSelection = updateSelection init'

clearSelection :: Interface -> Interface
clearSelection = updateSelection (const [])

isSelected :: Card -> Interface -> Bool
isSelected x i = x `elem` iSelection i

setGen :: StdGen -> Interface -> Interface
setGen g i = i { iStdGen = g }

-- Input loop and types

data Command = Move Direction | Select | Quit | Deal | DeleteLast | Hint
data Direction = GoUp | GoDown | GoLeft | GoRight

handleInput :: Vty -> IO Command
handleInput vty = do
 ev <- next_event vty
 case ev of
   EvKey KBS [] -> return DeleteLast
   EvKey KUp [] -> return (Move GoUp)
   EvKey KDown [] -> return (Move GoDown)
   EvKey KLeft [] -> return (Move GoLeft)
   EvKey KRight [] -> return (Move GoRight)
   EvKey KEnter [] -> return Select
   EvKey (KASCII 'd') [] -> return Deal
   EvKey (KASCII 'h') [] -> return Hint
   EvKey (KASCII 'q') [] -> return Quit
   _ -> handleInput vty

moveCur :: Direction -> Int -> CurrentControl -> CurrentControl
moveCur dir n (CardButton c) = correct $ case dir of
  GoUp -> c - width
  GoDown -> c + width
  GoLeft -> c - 1
  GoRight -> c + 1
 where
 width = 4
 correct i = CardButton (i `mod` n)

printGame :: Vty -> Interface -> IO Interface
printGame vty s = do
   update vty $ make_picture
              $ interfaceImage s
   case iTimer s of
     Nothing -> return s
     Just (delay, s') -> do
       threadDelay delay
       printGame vty s'

giveHint :: Game -> Interface -> Interface
giveHint game s = setGen g . f $ s
  where
  f = case mbHint of
        Just a -> setMessage hintmsg . setSelection [a]
        Nothing -> setMessage dealmsg

  (mbHint, g) = hint (iStdGen s) game

  hintmsg = "There is a set using this card."
  dealmsg = "No sets in this tableau, deal more cards."

-- | 'checkSet' will extract the chosen set from the tableau and check it
--   for validity. If a valid set is removed from the tableau the tableau
--   will be refilled up to 12 cards.
checkSet :: Card -> Card -> Card -> Game -> Interface
         -> (Game, Interface)
checkSet a b c game s = case considerSet a b c game of
  Nothing       -> (game, setMessage "Not a set!"
                        . delayedUpdate (seconds 1 `div` 4)
                        ( setMessage "Not a set.")
                        $ s)

  Just game'    -> (game', setMessage "Good job!"
                        . delayedUpdate (seconds 1)
                        ( setGame game'
                        . clearSelection
                        . setMessage "Good job.")
                        $ s)

checkNoSets :: Game -> Interface -> Maybe (Game, Interface)
checkNoSets game s = case extraCards game of
  Right game' -> Just (game', setGame game'
                            . incDealCounter
                            . setMessage "Dealing more cards."
                            $ s)
  Left 0 -> Nothing
  Left sets -> Just (game, incBadDealCounter
                         . setMessage msg
                         $ s)
    where
       msg = "Oops, " ++ show sets ++ " sets in tableau. Keep looking."

-------------------------------------------------------------------------------
-- Interface rendering functions ----------------------------------------------
-------------------------------------------------------------------------------

titleString :: String
titleString = "The game of Set"

helpString :: String
helpString = "(D)eal, (H)int, (Q)uit, Arrows move, Return selects,\
             \ Backspace unselects"

interfaceImage :: Interface -> Image
interfaceImage s =
  boldString (centerText 72 titleString)
  <->
  plainString helpString
  <->
  vert_cat (map cardRow rows)
  <->
  plainString "Cards remaining in deck: "
    <|> boldString  (leftPadText 2 (show (iRemaining s)))
    <|> plainString "    ["
    <|> boldString  (rightPadText 38 (iMessage s))
    <|> plainString "]"
  <->
  plainString "Extra deal count: "
    <|> boldString  (show (iDealCounter s))
    <|> plainString "/"
    <|> boldString  (show (iDealCounter s + iBadDealCounter s))
  <->
  plainString "Current Selection"
  <->
  cardRow (map ((,,) False False) (iSelection s))
  where
  plainString = string def_attr
  boldString = string (def_attr `with_style` bold)
  rows = groups 4 (zipWith testFocus [0..] (iTableau s))
  testFocus i c = (iControl s == CardButton i, isSelected c s, c)
  cardRow = horiz_cat . map cardimage

cardimage :: (Bool, Bool, Card) -> Image
cardimage (focused,selected,c)
     = char_fill fill_attr left_filler 1 (4 :: Int)
   <|> (vert_cat (map (Vty.string card_attr) xs))
   <|> char_fill fill_attr right_filler 1 (4 :: Int)
   <-> char_fill def_attr ' ' 18 (1 :: Int)
 where
  (card_color, xs) = cardLines c
  vty_color = case card_color of
    Red -> red
    Purple -> cyan
    Green -> green

  (fill_attr, left_filler, right_filler)
    | focused   = (def_attr`with_fore_color`white`with_back_color`yellow,
                     '▶', '◀')
    | otherwise = (def_attr, ' ', ' ')
  card_attr
    | selected = base_card_attr `with_style` reverse_video
    | otherwise = base_card_attr

  base_card_attr = def_attr `with_fore_color` vty_color `with_back_color` black

make_picture :: Image -> Picture
make_picture img = (pic_for_image img)
 { pic_background = Background ' ' def_attr }
