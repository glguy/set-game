module Set.Utils where

import Control.Monad                    (liftM)
import Data.List			(tails)
import System.Random                    (newStdGen, RandomGen, randomR)

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

-- | 'delete1' returns a list with the first occurrence of @x removed. If there
-- is no occurrence 'Nothing' is returned.
delete1 :: Eq a => a -> [a] -> Maybe [a]
delete1 x (y:ys)
  | x == y = Just ys
  | otherwise = fmap (y:) (delete1 x ys)
delete1 _ [] = Nothing

-- | 'index' returns the element at the given 0-based index and returns
-- 'Nothing' on failure.
index :: Int -> [a] -> Maybe a
index 0 (x:_)		= Just x
index n (_:xs) | n > 0	= index (n-1) xs
index _ _		= Nothing

select :: Int -> [a] -> (a,[a])
select _ [] = error "select: index too large"
select 0 (x:xs) = (x,xs)
select n (x:xs) = (y,x:ys)
  where
  (y,ys) = select (n-1) xs

-- | Drop last element of list if there is an element to drop.
init' :: [a] -> [a]
init' [] = []
init' xs = init xs

-------------------------------------------------------------------------------
-- Other utilities ------------------------------------------------------------
-------------------------------------------------------------------------------

-- | 'chooseThree' returns all combinations of three elements.
chooseTwo :: [a] -> [(a,a)]
chooseTwo xs = [ (a,b) | (a:as) <- tails xs
                       , b      <- as
                       ]

-- | 'seconds' converts seconds to microseconds for use in 'threadDelay'.
seconds :: Int -> Int
seconds x = 1000000 * x

-------------------------------------------------------------------------------
-- List shuffling utilities ---------------------------------------------------
-------------------------------------------------------------------------------

-- | 'shuffleIO' calls shuffle using a generator from 'newStdGen'.
shuffleIO :: [a] -> IO [a]
shuffleIO xs = liftM (fst . shuffle xs) newStdGen

-- | 'shuffle' shuffles the elements of a list using the given random generator.
shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle xs = shuffle' (length xs) xs

shuffle' :: RandomGen g =>  Int -> [a] -> g -> ([a], g)
shuffle' _ [] g = ([], g)
shuffle' n xs g = (x:xs'', g'')
  where
  n'		= n - 1
  (i, g')	= randomR (0,n') g
  (x, xs')	= select i xs
  (xs'', g'')	= shuffle' n' xs' g'

-------------------------------------------------------------------------------
-- Text manipulation utilities ------------------------------------------------
-------------------------------------------------------------------------------

-- | 'centerText' centers the given string in a field of @width characters.
centerText :: Int -> String -> String
centerText width xs = replicate ((width - length xs) `div` 2 ) ' ' ++ xs
                   ++ replicate ((width - length xs + 1) `div` 2 ) ' '
-- | 'centerText' right-aligns the given string in a field of @width characters.
leftPadText :: Int -> String -> String
leftPadText width xs = replicate (width - length xs) ' ' ++ xs

-- | 'centerText' left-aligns the given string in a field of @width characters.
rightPadText :: Int -> String -> String
rightPadText width xs = xs ++ replicate (width - length xs) ' '
