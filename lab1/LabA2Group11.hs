import Data.List
import System.Random
import Control.Parallel
import Control.DeepSeq
import Control.Parallel.Strategies
import Criterion.Main

-- Sequential sort for comparison
msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = [x]
msort xs = merge fh sh
  where fh = msort (take (length xs `div` 2) xs)
        sh = msort (drop (length xs `div` 2) xs)

-- Solution 1
sort1 :: Ord a => Int -> [a] -> [a]
sort1 t [] = []
sort1 t (x:[]) = [x]
sort1 t xs | length xs < t = msort xs
sort1 t xs = par (mforce fh) (pseq (mforce sh) (merge fh sh))
  where fh = sort1 t (take (length xs `div` 2) xs)
        sh = sort1 t (drop (length xs `div` 2) xs)

-- Solution 2
sort2 :: NFData a => Ord a => Int -> [a] -> [a]
sort2 t [] = []
sort2 t (x:[]) = [x]
sort2 t xs | length xs < t = msort xs
sort2 t xs = runEval $ do
  fh <- rparWith rdeepseq (sort2 t (take (length xs `div` 2) xs))
  sh <- rdeepseq (sort2 t (drop (length xs `div` 2) xs))
  rseq fh
  return (merge fh sh)



merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | (x >= y) = x:(merge xs (y:ys))
  | otherwise = y:(merge (x:xs) ys)


mforce :: [a] -> ()
mforce [] = ()
mforce (x:xs) = x `pseq` mforce xs

main = do
    let xs = take 200000(randoms (mkStdGen 211570155)) :: [Integer]
    let threshold = 1000
    putStrLn $ show (sum (sort2 threshold xs))
    defaultMain
        [
         bench "sort1" (nf (sort2 threshold) xs)
         ]
