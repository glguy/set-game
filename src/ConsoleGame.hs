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
  run game $ newInterfaceState vty
  shutdown vty

emptyGame :: Game -> Bool
emptyGame game = null (tableau game) && deckNull game

run :: Game -> InterfaceState -> IO ()
run game _ | emptyGame game = return ()
run game s = do
  printGame game s

  cmd <- handleInput s
  case cmd of
    Deal        -> checkNoSets game s
    DeleteLast  -> run game . initSelection
                            . clearMessage
                            $ s 
    Hint        -> giveHint s game
    Move dir    -> let f = moveCur dir (length (tableau game))
                   in run game (updateControl f s)
    Quit        -> return ()
    Select      -> select game s

select :: Game -> InterfaceState -> IO ()
select game s = case iControl s of
 CardButton i ->
  case index i (tableau game) of
   Just t | isSelected t s -> run game . updateSelection (delete t)
                                       . clearMessage
                                       $ s
          | otherwise    -> addCard t game s
   Nothing               -> run game (setControl (CardButton 0) s)

addCard :: Card -> Game -> InterfaceState -> IO ()
addCard card0 game s = case iSelection s of
  (_:_:_:_)      -> run game . setMessage "Selection full"
                             $ s
  [card1, card2] -> checkSet card0 card1 card2 game . appendSelection card0 
                                                    . clearMessage
                                                    $ s
  _              -> run game . appendSelection card0 
                             . clearMessage
                             $ s

-- InterfaceState manipulation functions

data InterfaceState = IS
  { iVty :: Vty
  , iControl :: CurrentControl
  , iSelection :: [Card]
  , iMessage :: String
  , iDealCounter :: Int
  , iBadDealCounter :: Int
  }

newInterfaceState :: Vty -> InterfaceState
newInterfaceState vty = IS
  { iVty = vty
  , iControl = CardButton 0
  , iSelection = []
  , iMessage = ""
  , iDealCounter = 0
  , iBadDealCounter = 0
  }

data CurrentControl = CardButton Int
 deriving (Eq)

incDealCounter :: InterfaceState -> InterfaceState
incDealCounter i = i { iDealCounter = iDealCounter i + 1 }

incBadDealCounter :: InterfaceState -> InterfaceState
incBadDealCounter i = i { iBadDealCounter = iBadDealCounter i + 1 }

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

isSelected :: Card -> InterfaceState -> Bool
isSelected x i = x `elem` iSelection i

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

printGame :: Game -> InterfaceState -> IO ()
printGame game s = update (iVty s) 
                 $ make_picture
                 $ interfaceImage game s

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
checkSet :: Card -> Card -> Card -> Game -> InterfaceState -> IO ()
checkSet a b c game s = run game' . f_s
  where
  (f_s,game') = case considerSet a b c game of
    Nothing -> (setMessage "Not a set.", game)
    Just game1 -> (setMessage "Good job." . clearSelection, game1)

checkNoSets :: Game -> InterfaceState -> IO ()
checkNoSets game = case extraCards game of
  Right game' -> run game' . incDealCounter
                           . setMessage "Dealing more cards."
  Left 0 -> const (return ()) -- game over
  Left sets -> run game . incBadDealCounter
                        . setMessage msg
    where
       msg = "Oops, " ++ show sets ++ " sets in tableau. Keep looking."

titleString :: String
titleString = centerText 72 "The game of Set"

helpString :: String
helpString = 
    "(D)eal, (H)int, (Q)uit, Arrows move, Return selects, Backspace unselects"

interfaceImage :: Game -> InterfaceState -> Image
interfaceImage game s =
  boldString titleString
  <->
  plainString helpString
  <->
  vert_cat (map cardRow rows)
  <->
  plainString "Cards remaining in deck: "
    <|> boldString  (leftPadText 2 (show (deckSize game)))
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
  rows = groups 4 (zipWith testFocus [0..] (tableau game))
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
