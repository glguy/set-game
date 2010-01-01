{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Set where

import Control.Monad                    (liftM, liftM4, unless)
import Data.Char                        (isSpace)
import Data.Function                    (on)
import Data.List                        (tails, transpose)
import MonadLib                         (StateM(..), runId, runStateT)
import System.Console.Editline.Readline (readline)
import System.Random                    (newStdGen, RandomGen, randomR)
import qualified System.Console.Terminfo as TI
  
data Color = Red | Purple | Green
 deriving (Show, Eq)

data Count = One | Two | Three
 deriving (Show, Eq)
 
data Shading = Open | Striped | Solid
 deriving (Show, Eq)
 
data Symbol = Diamond | Squiggle | Oval
 deriving (Show, Eq)

data Card = Card { color :: Color
                 , count :: Count
                 , shading :: Shading
                 , symbol :: Symbol
                 }
 deriving (Show, Eq)

valid :: Eq b => (a -> b) -> [(a,a)] -> Bool
valid f xs = all equal xs || all unequal xs
 where
 equal   (a,b)  = on (==) f a b
 unequal (a,b)  = on (/=) f a b

validSet :: Card -> Card -> Card -> Bool
validSet card1 card2 card3
  = valid color combos
 && valid count combos
 && valid shading combos
 && valid symbol combos
  where
  combos = chooseTwo [card1, card2, card3] 

allCards :: [Card]
allCards = liftM4 Card [Red,     Purple,   Green]
                       [One,     Two,      Three]
                       [Open,    Striped,  Solid]
                       [Diamond, Squiggle, Oval]


-- | 'solveBoard' returns the list of all valid sets contained in the given
--   list.
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

-- | 'tableauWidth' is the number of cards to be rendered in each row on the
--   tableau.
tableauWidth :: Int
tableauWidth = 3

-- | 'game' shuffles
game :: (?term :: TI.Terminal) => IO ()
game = game' [] =<< shuffleIO allCards

game' :: (?term :: TI.Terminal) => [Card] -> [Card] -> IO ()
game' [] [] = putStrLn "Game over!"
game' tableau deck = do
  (tableau', deck') <- deal tableau deck

  putStrLn ("Cards remaining deck: " ++ show (length deck'))
  putStr (renderTableau tableau')

  sel <- prompt "Selection:"
  case sel of
    Nothing                     -> return ()
    Just (0,0,0)                -> checkNoSets tableau' deck'
    Just (a,b,c)                -> checkSet a b c tableau' deck'
-- | 'deal' adds cards to the tableau from the deck up to a minimum of
--   'tableauSize' cards.
deal :: [Card] -> [Card] -> IO ([Card],[Card])
deal tableau deck = do
  unless (null dealt) (putStrLn ("Dealing " ++ show (length dealt) ++ " cards"))
  return (tableau ++ dealt, deck')
 where
  (dealt, deck')    = splitAt (tableauSize - length tableau) deck


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
       sets = length (solveBoard tableau)




-------------------------------------------------------------------------------
-- Card drawing functions -----------------------------------------------------
-------------------------------------------------------------------------------

-- | 'renderTableau' renders a list of 'Card's in a grid with 1-based indexing
--   as a 'String' using the current 'Terminal'.
renderTableau :: (?term :: TI.Terminal) => [Card] -> String
renderTableau = unlines
              . map concatGroup
              . groups tableauWidth
              . zipWith3 addIndex pads [1 :: Int ..]
              . map renderCard
  where
  pads = replicate 9 " " ++ repeat ""
  addIndex pad index = zipWith (++) ((pad ++ show index) : repeat "  ")

-- | 'renderCardRow' renders a list of 'Card's in a row without indexing.
renderCardRow :: (?term :: TI.Terminal) => [Card] -> String
renderCardRow = concatGroup . map padLines . map renderCard
  where padLines = map ("  "++)

concatGroup :: [[String]] -> String
concatGroup = unlines . map concat . transpose

-- | 'renderCard' renders a 'Card' to a list of lines with the appropriate art
--   and coloring and duplication.
renderCard :: (?term :: TI.Terminal) => Card -> [String]
renderCard Card {color, count, shading, symbol}
  = map (addColor color . duplicate count) (selectArt shading symbol)

-- | 'duplicate' pads a 'String' to fit neatly, centered in a 14-character
--   region.
duplicate :: Count -> String -> String
duplicate One   x = "      " ++        x        ++ "      "
duplicate Two   x = "   "    ++ x ++ "  " ++ x  ++    "   "
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

-- | 'groups' breaks a list into sublists of the given size. The final resulting
--   group may contain fewer elements than the given size.
--   Property: For all positive n. concat (groups n xs) == xs
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
  (x1,xs1) = select (dec a b b) xs0
  (x2,xs2) = select (dec a c (dec b c c)) xs1

  -- | 'dec' is used to correct indexes that will be affected
  -- by previous selects.
  dec x y z | x < y     = z - 1
            | otherwise = z

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
    _ -> do putStrLn "Bad input"
            prompt p

-- | '?=<<' is the monadic bind operator for MaybeT, except inlined to not
--   require a newtype.
(?=<<) :: Monad m => (a -> m (Maybe b)) -> m (Maybe a) -> m (Maybe b)
f ?=<< m = maybe (return Nothing) f =<< m

-- | 'curry3' converts an uncurried function on 3 arguments to a curried
--   function on a 3-tuple. 
curry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
curry3 f (a,b,c) = f a b c

-- | 'bounded' test that x is between lo and hi (inclusive).
bounded :: Ord a => a -> a -> a -> Bool
bounded lo hi x = lo <= x && x <= hi

-- | 'chooseThree' returns all combinations of three elements.
chooseTwo :: [a] -> [(a,a)]
chooseTwo xs = [ (a,b) | (a:as) <- tails xs
                       , b      <- as
                       ]

-- | 'chooseThree' returns all combinations of three elements.
chooseThree :: [a] -> [(a,a,a)]
chooseThree xs = [ (a,b,c) | (a:as) <- tails xs
                           , (b:bs) <- tails as
                           , c      <- bs
                           ]
