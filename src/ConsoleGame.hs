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
  run (IS vty (CardButton 0) [] "") game
  shutdown vty

emptyGame :: Game -> Bool
emptyGame game = null (tableau game) && deckNull game

run :: InterfaceState -> Game -> IO ()
run _ game | emptyGame game = return ()
run s game = do
  let tableauCards = tableau game
  printGame s tableauCards (deckSize game)

  cmd <- handleInput s
  case cmd of
    Deal	-> checkNoSets s game
    DeleteLast	-> run (initSelection (clearMessage s)) game
    Hint	-> giveHint s game
    Move dir	-> run (updateCur (moveCur dir (length tableauCards)) s) game
    Quit	-> return ()
    Select	-> select s game

select :: InterfaceState -> Game -> IO ()
select s@(IS _ (CardButton i) cards _) game =
 case index i (tableau game) of
  Just t
    | t `elem` cards    -> run (deleteSelection t (clearMessage s)) game
    | otherwise 	-> addCard t s game
  Nothing		-> run (focusCard 0 s) game

addCard :: Card -> InterfaceState -> Game -> IO ()
addCard _ s@(IS _ _ [_,_,_] _) = run (setMessage "Selection full" s)
addCard card0 s@(IS _ _ [card1, card2] _)
 = checkSet (appendSelection card0 (clearMessage s)) card0 card1 card2
addCard card0 s = run (appendSelection card0 (clearMessage s))

-- InterfaceState manipulation functions
clearMessage :: InterfaceState -> InterfaceState
clearMessage = setMessage ""

setMessage :: String -> InterfaceState -> InterfaceState
setMessage msg (IS vty cur sel _) = IS vty cur sel msg

focusCard :: Int -> InterfaceState -> InterfaceState
focusCard n (IS vty _ cards msg) = IS vty (CardButton n) cards msg

appendSelection :: Card -> InterfaceState -> InterfaceState
appendSelection card (IS vty cur cards msg) = IS vty cur (cards ++ [card]) msg

clearSelection :: InterfaceState -> InterfaceState
clearSelection (IS vty cur _ msg) = IS vty cur [] msg

deleteSelection :: Card -> InterfaceState -> InterfaceState
deleteSelection card (IS vty cur cards msg) = IS vty cur (delete card cards) msg

initSelection :: InterfaceState -> InterfaceState
initSelection s@(IS _ _ [] _) = s
initSelection (IS vty cur cards msg) = IS vty cur (init cards) msg

updateCur :: (CurrentControl -> CurrentControl)
          -> InterfaceState -> InterfaceState
updateCur f (IS vty cur cards msg) = IS vty (f cur) cards msg

-- Input loop and types
data Command = Move Direction | Select | Quit | Deal | DeleteLast | Hint
data Direction = GoUp | GoDown | GoLeft | GoRight

handleInput :: InterfaceState -> IO Command
handleInput s@(IS vty _ _ _) = do
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

printGame :: InterfaceState -> [Card] -> Int -> IO ()
printGame (IS vty cur selection msg) tab n = do
  update vty (make_picture (interfaceImage cur tab msg selection n))

giveHint :: InterfaceState -> Game -> IO ()
giveHint s game = do
  mbHint <- hint game
  case mbHint of
    Just a -> run (setMessage "There is a set using this card."
                  . appendSelection a
                  . clearSelection
                  $ s) game
    Nothing -> run (setMessage "No sets in this tableau, deal more cards." s)
                   game

-- | 'checkSet' will extract the chosen set from the tableau and check it
--   for validity. If a valid set is removed from the tableau the tableau
--   will be refilled up to 12 cards.
checkSet :: InterfaceState
         -> Card
         -> Card
         -> Card
         -> Game 
         -> IO ()
checkSet s a b c game = run (f_s s) game'
  where
  (f_s,game') = case considerSet a b c game of
    Nothing -> (setMessage "Not a set.", game)
    Just game1 -> (setMessage "Good job." .  clearSelection, game1)

checkNoSets :: InterfaceState -> Game -> IO ()
checkNoSets s game = case extraCards game of
  Right game' -> run (setMessage "Dealing more cards." s) game'
  Left 0 -> return ()
  Left sets -> run (setMessage msg s) game
    where
       msg = "Oops, " ++ show sets ++ " sets in tableau. Keep looking."

data InterfaceState = IS Vty CurrentControl [Card] String
data CurrentControl = CardButton Int
 deriving (Eq)

titleString :: String
titleString = centerText 72 "The game of Set"

interfaceImage :: CurrentControl -> [Card] -> String -> [Card] -> Int -> Image
interfaceImage cur cards msg selection deckRemaining =
  string (def_attr `with_style` bold) titleString
  <->
  string def_attr ("(D)eal, (H)int, (Q)uit, Arrows move, Return selects, Backspace unselects")
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
                    <-> char_fill def_attr ' ' 16 (1 :: Int)
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


centerText :: Int -> String -> String
centerText width xs = replicate ( (width - n) `div` 2 ) ' ' ++ xs
  where
  n = length xs

leftPadText :: Int -> String -> String
leftPadText n xs = replicate (n - length xs) ' ' ++ xs

rightPadText :: Int -> String -> String
rightPadText n xs = xs ++ replicate (n - length xs) ' '
