import Data.List
import System.Random
import Criterion.Main
import Control.Parallel
import Control.DeepSeq
import Control.Parallel.Strategies

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

-- map 12 sec
jackknife :: NFData b => ([a] -> b) -> [a] -> [b]
jackknife f = rmap f . resamples 500

jackknife2 :: NFData b => ([a] -> b) -> [a] -> [b]
jackknife2 f xs = (map f (resamples 500 xs)) `using` parListChunk 100 rseq

smap :: (a -> b) -> [a] -> [b]
smap f [] = []
smap f (x:xs) = par sf1 (pseq sf2 (sf1 : sf2))
  where sf1 = f x
        sf2 = smap f xs

s2map :: NFData b => Integer -> (a -> b) -> [a] -> [b]
s2map 0 f xs = map f xs
s2map d f [] = []
s2map d f xs = par (p1) (pseq (p2) (p1 ++ p2))
  where (h,t) = splitAt (div (length xs) 2) xs
        p1 = s2map (d-1) f h
        p2 = s2map (d-1) f t

rmap :: (a -> b) -> [a] -> [b]
rmap f [] = []
rmap f (x:xs) =  runEval $ do
    a <- rpar (f x)
    b <- rpar (rmap f xs)
    return (a : b)

rdmap :: Integer -> (a -> b) -> [a] -> [b]
rdmap 0 f xs = map f xs
rdmap d f [] = []
rdmap d f (x:[]) = [f x]
rdmap d f l = runEval $ do
  let (xs,ys) = splitAt ((length l + 1) `div` 2) l
  a <- rpar (rdmap (d-1) f xs)
  b <- rpar (rdmap (d-1) f ys)
  rseq a
  rseq b
  return (a ++ b)

crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]

main = do
  let (xs,ys) = splitAt 1500  (take 6000(randoms (mkStdGen 211570155)) :: [Float] )
  -- handy (later) to give same input different parallel functions

  let rs = crud xs ++ ys
  putStrLn $ "sample mean:    " ++ show (mean rs)

  let j = jackknife mean rs :: [Float]
  putStrLn $ "jack mean min:  " ++ show (minimum j) ++ " correct: " ++ show (minimum j == 0.5732047)
  putStrLn $ "jack mean max:  " ++ show (maximum j) ++ " correct: " ++ show (maximum j == 0.6492427)
  defaultMain
        [
         bench "jackknife" (nf (jackknife  mean) rs)
         ]


