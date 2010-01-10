module Main where

import Data.List                        (delete)
import Graphics.Vty as Vty
  
import Set.Ascii
import Set.Card (Card, Color(Red,Purple,Green))
import Set.GameLogic
import Set.Utils hiding (select)

main :: IO ()
main = do
  vty <- mkVty
  game <- newGame
  run game $ IS vty (CardButton 0) [] ""
  shutdown vty

emptyGame :: Game -> Bool
emptyGame game = null (tableau game) && deckNull game

run :: Game -> InterfaceState -> IO ()
run game _ | emptyGame game = return ()
run game s = do
  printGame s game

  cmd <- handleInput s
  case cmd of
    Deal        -> checkNoSets s game
    DeleteLast  -> run game . initSelection
                            . clearMessage
                            $ s 
    Hint        -> giveHint s game
    Move dir    -> let f = moveCur dir (length (tableau game))
                   in run game (updateControl f s)
    Quit        -> return ()
    Select      -> select s game

select :: InterfaceState -> Game -> IO ()
select s game = case iControl s of
 CardButton i ->
  case index i (tableau game) of
   Just t | t `elem` iSelection s -> run game . updateSelection (delete t)
                                              . clearMessage
                                              $ s
          | otherwise    -> addCard t s game
   Nothing               -> run game (setControl (CardButton 0) s)

addCard :: Card -> InterfaceState -> Game -> IO ()
addCard card0 s game = case iSelection s of
  (_:_:_:_)      -> run game (setMessage "Selection full" s)
  [card1, card2] -> checkSet (appendSelection card0 (clearMessage s))
                             card0 card1 card2 game
  _              -> run game . appendSelection card0 
                             . clearMessage
                             $ s

-- InterfaceState manipulation functions

data InterfaceState = IS
  { iVty :: Vty
  , iControl :: CurrentControl
  , iSelection :: [Card]
  , iMessage :: String
  }

data CurrentControl = CardButton Int
 deriving (Eq)

setControl :: CurrentControl -> InterfaceState -> InterfaceState
setControl x i = i { iControl = x }

updateControl :: (CurrentControl -> CurrentControl)
              -> InterfaceState -> InterfaceState
updateControl f i = setControl (f (iControl i)) i

clearMessage :: InterfaceState -> InterfaceState
clearMessage = setMessage ""

setMessage :: String -> InterfaceState -> InterfaceState
setMessage msg i = i { iMessage = msg }

setSelection :: [Card] -> InterfaceState -> InterfaceState
setSelection xs i = i { iSelection = xs }

updateSelection :: ([Card] -> [Card]) -> InterfaceState -> InterfaceState
updateSelection f i = setSelection (f (iSelection i)) i

appendSelection :: Card -> InterfaceState -> InterfaceState
appendSelection card = updateSelection (++ [card])

initSelection :: InterfaceState -> InterfaceState
initSelection = updateSelection init'

clearSelection :: InterfaceState -> InterfaceState
clearSelection = updateSelection (const [])

-- Input loop and types

data Command = Move Direction | Select | Quit | Deal | DeleteLast | Hint
data Direction = GoUp | GoDown | GoLeft | GoRight

handleInput :: InterfaceState -> IO Command
handleInput s = do
 ev <- next_event (iVty s)
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
   _ -> handleInput s

moveCur :: Direction -> Int -> CurrentControl -> CurrentControl
moveCur dir n (CardButton c) = correct $ case dir of
  GoUp -> c - width
  GoDown -> c + width
  GoLeft -> c - 1
  GoRight -> c + 1
 where
 width = 4
 correct i = CardButton (i `mod` n)

printGame :: InterfaceState -> Game -> IO ()
printGame (IS vty cur selection msg) game = do
  update vty (make_picture (interfaceImage cur tab msg selection n))
  where
  n = deckSize game
  tab = tableau game

giveHint :: InterfaceState -> Game -> IO ()
giveHint s game = do
  mbHint <- hint game
  case mbHint of
    Just a -> let hintmsg = "There is a set using this card."
              in run game . setMessage hintmsg
                          . setSelection [a]
                          $ s
    Nothing -> let dealmsg = "No sets in this tableau, deal more cards." 
               in run game $ setMessage dealmsg s

-- | 'checkSet' will extract the chosen set from the tableau and check it
--   for validity. If a valid set is removed from the tableau the tableau
--   will be refilled up to 12 cards.
checkSet :: InterfaceState -> Card -> Card -> Card -> Game -> IO ()
checkSet s a b c game = run game' (f_s s)
  where
  (f_s,game') = case considerSet a b c game of
    Nothing -> (setMessage "Not a set.", game)
    Just game1 -> (setMessage "Good job." . clearSelection, game1)

checkNoSets :: InterfaceState -> Game -> IO ()
checkNoSets s game = case extraCards game of
  Right game' -> run game' $ setMessage "Dealing more cards." s
  Left 0 -> return ()
  Left sets -> run game $ setMessage msg s
    where
       msg = "Oops, " ++ show sets ++ " sets in tableau. Keep looking."

titleString :: String
titleString = centerText 72 "The game of Set"

helpString :: String
helpString = 
    "(D)eal, (H)int, (Q)uit, Arrows move, Return selects, Backspace unselects"

interfaceImage :: CurrentControl -> [Card] -> String -> [Card] -> Int -> Image
interfaceImage cur cards msg selection deckRemaining =
  string (def_attr `with_style` bold) titleString
  <->
  string def_attr helpString
  <->
  vert_cat (map cardRow rows)
  <->
  string def_attr " "
  <->
  string def_attr ("Cards remaining in deck: ")
    <|> string (def_attr `with_style` bold)
         (leftPadText 2 (show deckRemaining))
    <|> string def_attr "    ["
    <|> string (def_attr `with_style` bold) (rightPadText 38 msg)
    <|> string def_attr "]"
  <->
  string def_attr "Current Selection"
  <->
  cardRow (map ((,) False) selection)
  where
  rows = groups 4 (zipWith testFocus [0..] cards)
  testFocus i c = (cur == CardButton i , c)
  cardRow = horiz_cat . map cardimage

cardimage :: (Bool, Card ) -> Image
cardimage (focused,c) = char_fill fill_attr left_filler 1 (4 :: Int)
                    <|> (vert_cat (map (Vty.string attr) xs))
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

  attr = def_attr `with_fore_color` vty_color `with_back_color` black

make_picture :: Image -> Picture
make_picture img = Picture
 { pic_cursor = NoCursor
 , pic_image = img
 , pic_background = Background ' ' def_attr }

-- Text format utilities

centerText :: Int -> String -> String
centerText width xs = replicate ( (width - n) `div` 2 ) ' ' ++ xs
  where
  n = length xs

leftPadText :: Int -> String -> String
leftPadText n xs = replicate (n - length xs) ' ' ++ xs

rightPadText :: Int -> String -> String
rightPadText n xs = xs ++ replicate (n - length xs) ' '

-- | Drop last element of list if there is an element to drop.
init' :: [a] -> [a]
init' [] = []
init' xs = init xs
