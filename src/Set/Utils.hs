module Set.Utils where

import Control.Monad                    (liftM)
import Data.List			(tails)
import MonadLib                         (StateM(..), runId, runStateT)
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

-------------------------------------------------------------------------------
-- List shuffling utilities ---------------------------------------------------
-------------------------------------------------------------------------------

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
