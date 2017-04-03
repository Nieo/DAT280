import Data.List
import System.Random
import Criterion.Main
import Control.Parallel
import Control.DeepSeq
import Control.Parallel.Strategies
import Control.Monad.Par

-- code borrowed from the Stanford Course 240h (Functional Systems in Haskell)
-- I suspect it comes from Bryan O'Sullivan, author of Criterion

data T a = T !a !Int


mean :: (RealFrac a) => [a] -> a
mean = fini . foldl' go (T 0 0)
  where
    fini (T a _) = a
    go (T m n) x = T m' n'
      where m' = m + (x - m) / fromIntegral n'
            n' = n + 1


resamples :: Int -> [a] -> [[a]]
resamples k xs =
    take (length xs - k) $
    zipWith (++) (inits xs) (map (drop k) (tails xs))

jackknife :: NFData b => ([a] -> b) -> [a] -> [b]
jackknife f = dmap f . resamples 500

-- Task 1.c
cjackknife :: NFData b => ([a] -> b) -> [a] -> [b]
cjackknife f xs = (map f (resamples 500 xs)) `using` parListChunk 500 rseq

-- Task 1.a
amap :: (a -> b) -> [a] -> [b]
amap f [] = []
amap f (x:xs) = par sf1 (pseq sf2 (sf1 : sf2))
  where sf1 = f x
        sf2 = amap f xs

-- Task 1.a with granularity control
a2map :: NFData b => Integer -> (a -> b) -> [a] -> [b]
a2map 0 f xs = map f xs
a2map d f [] = []
a2map d f xs = par (force p1) (pseq (force p2) (p1 ++ p2))
  where (h,t) = splitAt (div (length xs) 2) xs
        p1 = a2map (d-1) f h
        p2 = a2map (d-1) f t

-- Task 1.b
bmap :: (a -> b) -> [a] -> [b]
bmap f [] = []
bmap f (x:xs) =  runEval $ do
    a <- rpar (f x)
    b <- rseq (bmap f xs)
    rseq a
    return (a : b)

-- Task 1.b with granularity control
b2map :: Integer -> (a -> b) -> [a] -> [b]
b2map 0 f xs = map f xs
b2map d f [] = []
b2map d f (x:[]) = [f x]
b2map d f l = runEval $ do
  let (xs,ys) = splitAt ((length l + 1) `div` 2) l
  a <- rpar (b2map (d-1) f xs)
  b <- rpar (b2map (d-1) f ys)
  rseq a
  rseq b
  return (a ++ b)

-- Task 1.d
dmap :: NFData b => (a -> b) -> [a] -> [b]
dmap f [] = []
dmap f xs = runPar $ do
  xs' <- dmap' (return . f ) xs
  return xs'

dmap' :: NFData b => (a -> Par b) -> [a] -> Par [b]
dmap' f xs = do
  is <- mapM (spawn . f) xs
  mapM get is


crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]

main = do
  let (xs,ys) = splitAt 1500
        (take 6000(randoms (mkStdGen 211570155)) :: [Float] )
  -- handy (later) to give same input different parallel functions

  let rs = crud xs ++ ys
  putStrLn $ "sample mean:    " ++ show (mean rs)

  let j = jackknife mean rs :: [Float]
  putStrLn $ "jack mean min:  " ++ show (minimum j)
  putStrLn $ "jack mean max:  " ++ show (maximum j)
  defaultMain
        [
         bench "jackknife" (nf (jackknife  mean) rs)
         ]

