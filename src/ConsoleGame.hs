module Main where

import Control.Applicative              ((<$),(<$>), Applicative(..),(<*))
import Control.Monad                    (ap, liftM2)
import Data.Char                        (isSpace)
import Data.Ord                         (comparing)
import Data.List                        (delete)
import Data.Monoid                      (mconcat)
import Graphics.Vty as Vty
  
import Set.Ascii
import Set.Card
import Set.GameLogic
import Set.Utils hiding (select)

main :: IO ()
main = do
  vty <- mkVty
  game <- newGame
  run (IS vty (CardButton 0) [] "") game
  shutdown vty

make_picture img = Picture
 { pic_cursor = NoCursor
 , pic_image = img
 , pic_background = Background ' ' def_attr }

emptyGame :: Game -> Bool
emptyGame game = null (tableau game) && deckNull game

run :: InterfaceState -> Game -> IO ()
run s game | emptyGame game = return ()
run s game = do
  let tableauCards = tableau game
  printGame s tableauCards (deckSize game)

  cmd <- handleInput s
  case cmd of
    Deal	-> checkNoSets s game
    DeleteLast	-> run (initSelection s) game
    Hint	-> giveHint s game
    Move dir	-> run (updateCur (moveCur dir (length tableauCards)) s) game
    Quit	-> return ()
    Select	-> select s game

select s@(IS vty (CardButton i) cards msg) game =
 case index i (tableau game) of
  Just t
    | t `elem` cards    -> run (deleteSelection t s) game
    | otherwise 	-> addCard t s game
  Nothing		-> run (focusCard 0 s) game

focusCard n (IS vty _ cards msg) = IS vty (CardButton n) cards msg
appendSelection card (IS vty cur cards msg) = IS vty cur (cards ++ [card]) msg
clearSelection (IS vty cur _ msg) = IS vty cur [] msg
deleteSelection card (IS vty cur cards msg) = IS vty cur (delete card cards) msg
initSelection s@(IS _ _ [] _) = s
initSelection (IS vty cur cards msg) = IS vty cur (init cards) msg

addCard _ s@(IS _ _ [_,_,_] _) game = run (setMessage "Selection full" s) game
addCard card0 s@(IS _ _ [card1, card2] _) game
 = checkSet (appendSelection card0 s) card0 card1 card2 game
addCard card0 s game
 = run (appendSelection card0 s) game

updateCur f (IS vty cur cards msg) = IS vty (f cur) cards msg

data Command = Move Direction | Select | Quit | Deal | DeleteLast | Hint
data Direction = GoUp | GoDown | GoLeft | GoRight

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

example =
  let xs = liftM2 (,) [Red,Purple,Green] [One,Two,Three]
      ys = liftM2 (,) [Diamond, Squiggle, Oval] [Solid, Striped, Open]
      cards = zipWith (\ (col,cou) (shap, shad) -> Card col cou shad shap) xs ys
  in ()


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
    Just game' -> (setMessage "Good job." .  clearSelection, game')

checkNoSets :: InterfaceState -> Game -> IO ()
checkNoSets s game = case extraCards game of
  Right game' -> run (setMessage "Dealing more cards" s) game'
  Left 0 -> return ()
  Left sets -> do
       let msg = "Oops, " ++ show sets ++ " sets in tableau. Keep looking."
       run (setMessage msg s) game

data InterfaceState = IS Vty CurrentControl [Card] String
data CurrentControl = CardButton Int
 deriving (Eq)

interfaceImage cur cards msg selection deckRemaining =
  string def_attr "The game of Set"
  <->
  vert_cat (map (cardRow cur) rows)
  <->
  string (def_attr `with_style` bold) (if null msg then " " else msg)
  <->
  string def_attr " "
  <->
  string def_attr ("Cards remaining in deck: ")
    <|> string (def_attr `with_style` bold) (show deckRemaining)
  <->
  string def_attr " "
  <->
  string def_attr "Current Selection"
  <->
  cardRow cur (map ((,) (-1)) selection)
  where
  rows = groups 4 (zip [0..] cards)

cardRow cur row = horiz_cat (map (cardimage cur) row)

cardimage :: CurrentControl -> (Int, Card ) -> Image
cardimage cur (i,c) = char_fill fill_attr ' ' 1 4
               <|> (vert_cat (map (Vty.string attr) xs))
               <|> char_fill fill_attr ' ' 1 4
               <-> char_fill def_attr ' ' 16 1
  where
  (color, xs) = cardLines c
  vty_color = case color of
    Red -> red
    Purple -> cyan
    Green -> green

  fill_attr
    | cur == CardButton i = def_attr `with_back_color` yellow
    | otherwise = def_attr
    
  attr = def_attr `with_fore_color` vty_color `with_back_color` black

clearMessage = setMessage ""
setMessage msg (IS vty cur sel _) = IS vty cur sel msg
