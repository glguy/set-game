module Main where

import Control.Concurrent               (threadDelay)
import Data.List                        (delete)
import Data.Foldable                    (traverse_)
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

-- | 'run' is the main event-loop for the game. It alternates
--   reading user input and drawing the interface.
run :: Vty -> Game -> Interface -> IO ()
run vty game s
 | emptyGame game = return ()
 | otherwise = do
  s' <- printGame vty s

  cmd <- handleInput vty
  let simple f = Just (game, f s')
  traverse_ (uncurry (run vty)) $ case cmd of
        Deal        -> checkNoSets game s'
        DeleteLast  -> simple $ initSelection . clearMessage
        Hint        -> simple $ giveHint game
        Move dir    -> simple $ moveFocus dir
        Quit        -> Nothing
        Select      -> select game s'

-- | 'select' performs an event on the interface based on the currently
--   focused control.
select :: Game -> Interface -> Maybe (Game, Interface)
select game s = case iControl s of
 CardButton i -> case index i (iTableau s) of
   Just t | isSelected t s      -> Just (game, updateSelection (delete t)
                                             . clearMessage
                                             $ s)
          | otherwise           -> addCard t game s
   Nothing                      -> Just (game, setControl (CardButton 0)
                                             $ s)

-- | 'addCard' attempts to add a new card to the selection and handles
--   checking for a valid set when the selection becomes full.
addCard :: Card -> Game -> Interface -> Maybe (Game, Interface)
addCard card0 game s = case iSelection s of
  (_:_:_:_)      -> Just (game, setMessage "Selection full" s)
  [card1, card2] -> checkSet card0 card1 card2 game . appendSelection card0
                                                    . clearMessage
                                                    $ s
  _              -> Just (game, appendSelection card0
                              . clearMessage
                              $ s)

-- | 'setGame' updates the 'Interface' based on the current 'Game' state.
setGame :: Game -> Interface -> Interface
setGame game = setTableau (tableau game)
             . setRemaining (deckSize game)

moveFocus :: Direction -> Interface -> Interface
moveFocus dir s = setControl control s
 where
  control = case iControl s of
    CardButton i -> moveFocusCardButton dir i (length (iTableau s))

moveFocusCardButton :: Direction -> Int -> Int -> CurrentControl
moveFocusCardButton dir i n = CardButton i1
   where
    (row,col) = i `divMod` cols

    (row1, col1) = case dir of
      GoUp        -> (row - 1, col)
      GoDown      -> (row + 1, col)
      GoLeft      -> (row, col - 1)
      GoRight     -> (row, col + 1)

    i1 = case dir of
      _ | toI (row1, col1) < n
                  -> toI (row1, col1)
      GoUp        -> toI (-2, col1)
      GoDown      -> toI (0, col1)
      GoLeft      -> toI (row1, n-1)
      GoRight     -> toI (row1, 0)

    toI (r,c) = r `mod` rows * cols + c `mod` cols

    rows = (n + cols - 1) `div` cols
    cols = tableauWidth

printGame :: Vty -> Interface -> IO Interface
printGame vty s = do
   update vty $ make_picture
              $ interfaceImage s
   case iTimer s of
     Nothing -> return s
     Just (delay, s') -> do
       threadDelay delay
       printGame vty s'

-- | 'giveHint' checks for a hint in the current game and alters the
--   selection if a hint is found.
giveHint :: Game -> Interface -> Interface
giveHint game s = incHintCounter . setGen g . f $ s
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
         -> Maybe (Game, Interface)
checkSet a b c game s = Just $ case considerSet a b c game of
  Nothing       -> (game, setMessage "Not a set!"
                        . delayedUpdate (seconds 1 `div` 4)
                        ( setMessage "Not a set.")
                        $ s)

  Just game'    -> (game', delayedUpdate (seconds 1)
                        ( setGame game'
                        . clearSelection
                        . setMessage "Good job.")
                        . setMessage "Good job!"
                        $ s)

-- | 'checkNoSets' verifies that there are no sets in the current tableau
-- and deals additional cards to the tableau in that case.
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
-- Interface manipulation functions -------------------------------------------
-------------------------------------------------------------------------------

data Interface = IS
  { iControl :: CurrentControl
  , iSelection :: [Card]
  , iMessage :: String
  , iDealCounter :: Integer
  , iBadDealCounter :: Integer
  , iHintCounter :: Integer
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
  , iHintCounter = 0
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

delayedUpdate :: Int -> (Interface -> Interface)
              -> Interface -> Interface
delayedUpdate delay f s = setTimer delay (f s) s

incHintCounter :: Interface -> Interface
incHintCounter i = i { iHintCounter = iHintCounter i + 1 }

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

-------------------------------------------------------------------------------
-- Input to even mapping ------------------------------------------------------
-------------------------------------------------------------------------------

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
  tableauImage s
  <->
  plainString "Cards remaining in deck: "
    <|> boldString  (leftPadText 2 (show (iRemaining s)))
    <|> plainString "    ["
    <|> boldString  (rightPadText 38 (iMessage s))
    <|> plainString "]"
  <->
  plainString "Deal count: "
    <|> boldString  (show (iDealCounter s))
    <|> plainString "/"
    <|> boldString  (show (iDealCounter s + iBadDealCounter s))
    <|> plainString "   Hints used: "
    <|> boldString  (show (iHintCounter s))
  <->
  plainString "Current Selection"
  <->
  cardRowImage (map ((,,) False False) (iSelection s))

tableauImage :: Interface -> Image
tableauImage s = vert_cat
               $ map cardRowImage
               $ groups tableauWidth 
               $ zipWith testFocus [0..] 
               $ iTableau s
  where
  testFocus i c = (iControl s == CardButton i, isSelected c s, c)

cardRowImage :: [(Bool, Bool, Card)] -> Image
cardRowImage = horiz_cat . map cardImage

-- | 'cardImage' renders a 'Card' based on its current selection and focus
--   state.
cardImage :: (Bool, Bool, Card) -> Image
cardImage (focused,selected,c) = left_side <|> body <|> right_side <-> bottom
 where
  body          = vert_cat (map (Vty.string card_attr) xs)
  left_side     = char_fill fill_attr left_filler 1 (4 :: Int)
  right_side    = char_fill fill_attr right_filler 1 (4 :: Int)
  bottom        = char_fill def_attr ' ' 18 (1 :: Int)

  (card_color, xs) = cardLines c

  vty_color = case card_color of
    Red         -> red
    Purple      -> cyan
    Green       -> green

  (fill_attr, left_filler, right_filler)
    | focused   = (def_attr`with_fore_color`white`with_back_color`yellow,
                     '▶', '◀')
    | otherwise = (def_attr, ' ', ' ')

  card_attr
    | selected  = base_card_attr `with_style` reverse_video
    | otherwise = base_card_attr

  base_card_attr = def_attr `with_fore_color` vty_color `with_back_color` black

make_picture :: Image -> Picture
make_picture img = (pic_for_image img)
 { pic_background = Background ' ' def_attr }

plainString :: String -> Image
plainString = string def_attr

boldString :: String -> Image
boldString = string (def_attr `with_style` bold)

tableauWidth :: Int
tableauWidth = 4
