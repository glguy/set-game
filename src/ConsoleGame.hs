module Main where

import Data.List                        (delete)
import Graphics.Vty as Vty
import System.Random (newStdGen, StdGen)
  
import Set.Ascii
import Set.Card (Card, Color(Red,Purple,Green))
import Set.GameLogic
import Set.Utils hiding (select)

main :: IO ()
main = do
  vty <- mkVty
  game <- newGame
  g <- newStdGen
  run vty game $ newInterfaceState g
  shutdown vty

emptyGame :: Game -> Bool
emptyGame game = null (tableau game) && deckNull game

run :: Vty -> Game -> InterfaceState -> IO ()
run _ game _ | emptyGame game = return ()
run vty game s = do
  printGame vty game s

  cmd <- handleInput vty
  let simple f = Just (game, f s)
  let res = case cmd of
        Deal        -> checkNoSets game s
        DeleteLast  -> simple $ initSelection
                              . clearMessage
        Hint        -> simple $ giveHint game
        Move dir    -> simple $ updateControl $ moveCur dir $ length
                              $ tableau game
        Quit        -> Nothing
        Select      -> Just (select game s)
  case res of
    Nothing -> return ()
    Just (game', s') -> run vty game' s'

select :: Game -> InterfaceState -> (Game, InterfaceState)
select game s = case iControl s of
 CardButton i ->
  case index i (tableau game) of
   Just t | isSelected t s -> (game, updateSelection (delete t)
                                   . clearMessage
                                   $ s)
          | otherwise    -> addCard t game s
   Nothing               -> (game, setControl (CardButton 0) s)

addCard :: Card -> Game -> InterfaceState -> (Game, InterfaceState)
addCard card0 game s = case iSelection s of
  (_:_:_:_)      -> (game, setMessage "Selection full" s)
  [card1, card2] -> checkSet card0 card1 card2 game . appendSelection card0 
                                                    . clearMessage
                                                    $ s
  _              -> (game, appendSelection card0 
                         . clearMessage
                         $ s)

-- InterfaceState manipulation functions

data InterfaceState = IS
  { iControl :: CurrentControl
  , iSelection :: [Card]
  , iMessage :: String
  , iDealCounter :: Int
  , iBadDealCounter :: Int
  , iStdGen :: StdGen
  }

newInterfaceState :: StdGen -> InterfaceState
newInterfaceState g = IS
  { iControl = CardButton 0
  , iSelection = []
  , iMessage = ""
  , iDealCounter = 0
  , iBadDealCounter = 0
  , iStdGen = g
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

setGen :: StdGen -> InterfaceState -> InterfaceState
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

printGame :: Vty -> Game -> InterfaceState -> IO ()
printGame vty game s = update vty 
                 $ make_picture
                 $ interfaceImage game s

giveHint :: Game -> InterfaceState -> InterfaceState
giveHint game s =
  let (mbHint, g) = hint (iStdGen s) game
  in case mbHint of
    Just a -> let hintmsg = "There is a set using this card."
              in setGen g . setMessage hintmsg . setSelection [a] $ s
    Nothing -> let dealmsg = "No sets in this tableau, deal more cards." 
               in setGen g . setMessage dealmsg $ s

-- | 'checkSet' will extract the chosen set from the tableau and check it
--   for validity. If a valid set is removed from the tableau the tableau
--   will be refilled up to 12 cards.
checkSet :: Card -> Card -> Card -> Game -> InterfaceState
         -> (Game, InterfaceState)
checkSet a b c game s = (game', f_s s)
  where
  (f_s,game') = case considerSet a b c game of
    Nothing -> (setMessage "Not a set.", game)
    Just game1 -> (setMessage "Good job." . clearSelection, game1)

checkNoSets :: Game -> InterfaceState -> Maybe (Game, InterfaceState)
checkNoSets game s = case extraCards game of
  Right game' -> Just (game', incDealCounter
                            . setMessage "Dealing more cards."
                            $ s)
  Left 0 -> Nothing
  Left sets -> Just (game, incBadDealCounter
                         . setMessage msg
                         $ s)
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
