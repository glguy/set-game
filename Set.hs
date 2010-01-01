{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Set where

import Control.Monad (forM_, liftM, unless)
import Data.List (tails, transpose)
import System.Random (RandomGen, randomR, newStdGen)
import MonadLib (runStateT, runId, StateM(..))
import qualified System.Console.Terminfo as TI
import System.Console.Editline.Readline (readline)
import Data.Char (isSpace)
  
data Color = Red | Purple | Green
 deriving (Show, Eq)

data Count = One | Two | Three
 deriving (Show, Eq)
 
data Shading = Open | Striped | Solid
 deriving (Show, Eq)
 
data Symbol = Diamond | Squiggle | Oval
 deriving (Show, Eq)

data Card = Card Color Count Shading Symbol
 deriving (Show, Eq)

valid :: Eq a => a -> a -> a -> Bool
valid a b c
  | a == b    = b == c
  | otherwise = a /= c && b /= c

validSet :: Card -> Card -> Card -> Bool
validSet (Card color1 count1 shading1 symbol1)
         (Card color2 count2 shading2 symbol2)
         (Card color3 count3 shading3 symbol3)
         =  valid color1 color2 color3
         && valid count1 count2 count3
         && valid shading1 shading2 shading3
         && valid symbol1 symbol2 symbol3

allCards :: [Card]
allCards = [ Card color count shading symbol
           | color   <- [Red,     Purple,   Green]
           , count   <- [One,     Two,      Three]
           , shading <- [Open,    Striped,  Solid]
           , symbol  <- [Diamond, Squiggle, Oval]
           ]

-- | 'chooseThree' returns all combinations of three elements.
chooseThree :: [a] -> [(a,a,a)]
chooseThree xs = [ (a,b,c) | (a:as) <- tails xs
                           , (b:bs) <- tails as
                           , c      <- bs
                           ]

solveBoard :: [Card] -> [(Card,Card,Card)]
solveBoard = filter (curry3 validSet) . chooseThree
            
-- | 'shuffleIO' calls shuffle using a generator from 'newStdGen'.
shuffleIO :: [a] -> IO [a]
shuffleIO xs = liftM (fst . shuffle xs) newStdGen

-- | 'shuffle' shuffles the elements of a list using the given random generator.
shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle xs g = runId (runStateT g (shuffle' (length xs) xs))

shuffle' :: (RandomGen g , StateM m g) =>  Int -> [a] -> m [a]
shuffle' _ [] = return []
shuffle' n xs = do
  let n' = n - 1
  i <- getRandom n'
  let (x, xs') = select i xs
  xs'' <- shuffle' n' xs'
  return (x:xs'')

getRandom :: (RandomGen g, StateM m g) => Int -> m Int
getRandom b = do
  (i, g') <- randomR (0,b) `liftM` get
  set g'
  return i

main :: IO ()
main = do
  term <- TI.setupTermFromEnv
  let ?term = term
  game

-- | 'tableauSize' is the minimum number of cards that should be on the tableau.
tableauSize :: Int
tableauSize = 12

-- | 'game' shuffles
game :: (?term :: TI.Terminal) => IO ()
game = game' [] =<< shuffleIO allCards

game' :: (?term :: TI.Terminal) => [Card] -> [Card] -> IO ()
game' tableau deck = do
  (tableau, deck) <- deal tableau deck

  -- Display current tableau
  putStrLn ("Cards remaining deck: " ++ show (length deck))
  printCards tableau
  putStrLn ""

  -- Handle user input
  sel <- prompt "Selection:"
  case sel of
    Nothing     				-> return ()
    Just (0,0,0)				-> checkNoSets tableau deck
    Just (a,b,c) | not (validInput a b c)	-> checkSet a b c tableau deck
                 | otherwise                    -> do putStrLn "Invalid input"
                 				      game' tableau deck
  where
  n = length tableau

  validInput a b c = a /= b && b /= c && a /= c && all inbounds [a,b,c]

  inbounds x = x > 0 && x <= n

-- | 'deal' adds cards to the tableau from the deck up to a minimum of
--   'tableauSize' cards.
deal :: [Card] -> [Card] -> IO ([Card],[Card])
deal tableau deck = do
  let cardsNeeded       = tableauSize - length tableau
      (dealt, deck')    = splitAt cardsNeeded deck
      tableau'          = tableau ++ dealt
  unless (null dealt) $
    putStrLn ("Dealing " ++ show (length dealt) ++ " cards")
  return (tableau', deck')


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
  printCardRow [card0, card1, card2]
  putStrLn message
  game' nextTableau deck
  where
  (card0, card1, card2, tableau') = select3 (a-1) (b-1) (c-1) tableau

  (message, nextTableau)
    | validSet card0 card1 card2        = ("Good job\n", tableau')
    | otherwise                         = ("Not a set!\n", tableau)


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
       sets = length (solveBoard tableau)




-------------------------------------------------------------------------------
-- Card drawing functions -----------------------------------------------------
-------------------------------------------------------------------------------

-- | Print a list of 'Card's in a grid with 1-based indexing
printCards :: (?term :: TI.Terminal) => [Card] -> IO ()
printCards cards = do
  let indexes = zipWith (++) (replicate 9 " " ++ repeat "")
                             (map show [1 :: Int ..])
  let cardLines = map renderCard cards
  let indexFirstLine i = zipWith (++) (i : repeat "  ")
  let indexedCardLines = zipWith indexFirstLine indexes cardLines
  forM_ (groups 3 indexedCardLines) $ \ x -> do
    mapM_ (putStrLn . concat) (transpose x)
    putStrLn ""

-- | Print a list of 'Card's in a row without indexing
printCardRow :: (?term :: TI.Terminal) => [Card] -> IO ()
printCardRow = mapM_ (putStrLn . concatPad) . transpose . map renderCard
  where concatPad = concatMap ("  "++)

-- | 'renderCard' renders a 'Card' to a list of lines with the appropriate art
--   and coloring and duplication.
renderCard :: (?term :: TI.Terminal) => Card -> [String]
renderCard (Card color count shading symbol)
  = map (addColor color . duplicate count) (selectArt shading symbol)

-- | 'duplicate' pads a 'String' to fit neatly, centered in a 14-character
--   region.
duplicate :: Count ->    (String -> String)

duplicate One   x = "      " ++ x ++ "      "
duplicate Two   x = "   " ++ x ++ "  " ++ x ++ "   "
duplicate Three x = " " ++ x ++ " " ++ x ++ " " ++ x ++ " "

addColor :: (?term :: TI.Terminal) => Color -> String -> String
addColor color = f2 TI.Black . f1 termcolor
  where
  termcolor = case color of
    Green  -> TI.Green
    Purple -> TI.Cyan
    Red    -> TI.Red

  Just f1 = TI.getCapability ?term TI.withForegroundColor
  Just f2 = TI.getCapability ?term TI.withBackgroundColor

-- | 'selectArt' returns the ASCII art representation of the lines for
--   a given 'Shading' and 'Symbol'.
selectArt :: Shading -> Symbol -> [String]
selectArt Open    Diamond  = ["    "
                             ," ╱╲ "
                             ,"╱  ╲"
                             ,"‾‾‾‾"]
selectArt Striped Diamond  = ["    "
                             ," ╱╲ "
                             ,"╱╱╲╲"
                             ,"‾‾‾‾"]
selectArt Solid   Diamond  = ["    "
                             ," ╱╲ "
                             ,"╱╳╳╲"
                             ,"‾‾‾‾"]
selectArt Open    Squiggle = ["___ "
                             ,"╲  ╲"
                             ,"╱  ╱"
                             ,"‾‾‾ "]
selectArt Striped Squiggle = ["___ "
                             ,"╲╲╲╲"
                             ,"╱╱╱╱"
                             ,"‾‾‾ "]
selectArt Solid   Squiggle = ["___ "
                             ,"╲╳╳╲"
                             ,"╱╳╳╱"
                             ,"‾‾‾ "]
selectArt Open    Oval     = [" __ "
                             ,"╱  ╲"
                             ,"╲  ╱"
                             ," ‾‾ "]
selectArt Striped Oval     = [" __ "
                             ,"╱╱╲╲"
                             ,"╲╲╱╱"
                             ," ‾‾ "]
selectArt Solid   Oval     = [" __ "
                             ,"╱╳╳╲"
                             ,"╲╳╳╱"
                             ," ‾‾ "]

-------------------------------------------------------------------------------
-- List utilities--------------------------------------------------------------
-------------------------------------------------------------------------------

groups :: Int -> [a] -> [[a]]
groups _ [] = []
groups n xs = as : groups n bs
  where
  (as,bs) = splitAt n xs

select :: Int -> [a] -> (a,[a])
select _ [] = error "select: index too large"
select 0 (x:xs) = (x,xs)
select n (x:xs) = (y,x:ys)
  where
  (y,ys) = select (n-1) xs

select3 :: Int -> Int -> Int -> [a] -> (a,a,a,[a])
select3 a b c xs = (x0,x1,x2,xs2)
  where
  (x0,xs0) = select a xs
  (x1,xs1) = select (dec a b) xs0
  (x2,xs2) = select (dec a (dec b c)) xs1

  dec x y | x < y     = y - 1
          | otherwise = y

-------------------------------------------------------------------------------
-- Other utilities ------------------------------------------------------------
-------------------------------------------------------------------------------

-- | 'prompt' wraps 'readline' with a 'Read' parser and repeats the prompt
--   on a failed parse.
prompt :: Read a => String -> IO (Maybe a)
prompt p = parseLn ?=<< readline p
  where
  parseLn ln = case reads ln of
    [(x,white)] | all isSpace white -> return (Just x)
    _ -> prompt p

-- | '?=<<' is the monadic bind operator for MaybeT, except inlined to not
--   require a newtype.
(?=<<) :: Monad m => (a -> m (Maybe b)) -> m (Maybe a) -> m (Maybe b)
f ?=<< m = maybe (return Nothing) f =<< m

-- | 'curry3' converts an uncurried function on 3 arguments to a curried
--   function on a 3-tuple. 
curry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
curry3 f (a,b,c) = f a b c
