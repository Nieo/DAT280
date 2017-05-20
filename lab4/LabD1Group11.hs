import Data.List
import System.Random

import Data.Array.Repa as Repa
import Data.Array.Repa.Eval
import Data.Vector.Unboxed.Base
import Criterion.Main

-- (buy, sell, profit)

buySell ::(Array U DIM1 Int) -> String--m(Array U DIM0 (Int, Int, Int))
buySell arr = show(Repa.foldP maxProfit (0,0,0) (indexed arr) :: Maybe (Array U DIM0(Int,Int,Int)) )

maxProfit :: (Int, Int, Int) -> (Int, Int, Int) -> (Int,Int,Int)
maxProfit (aBuy,aSell,aProfit) (bBuy,bSell,bProfit)
    | aProfit > bProfit = (aBuy,aSell,aProfit)
    | bProfit > aProfit = (bBuy,bSell,bProfit)
    | aSell - aBuy < bSell - bBuy = (aBuy,aSell,aProfit)
    | otherwise = (bBuy,bSell,bProfit)

mmax :: (Array U DIM1 Int) -> Int -> (Int,Int,Int)
mmax arr i = (i,sell,profit)
    where
        (price, sell) = maxafter arr i i
        profit = price - arr ! (Z:.i)


maxafter :: (Array U DIM1 Int) -> Int -> Int -> (Int, Int)
maxafter arr c i
    | c == size(extent arr)-1 = (arr ! (Z:.c), c)
    | c < i = maxafter arr (c+1) i
    | otherwise = gt (arr ! (Z:.c), c) (maxafter arr (c+1) i)

gt :: (Int,Int) -> (Int, Int) -> (Int, Int)
gt (a,aa) (b,bb)
    | b > a = (b,bb)
    | otherwise = (a,aa)

indexed :: (Array U DIM1 Int) -> (Array D DIM1 (Int,Int,Int))
indexed arr = Repa.traverse arr id (\src idx@(Z :. i) -> mmax arr i)

main = do
    let ins = fromList (Z :. (8::Int)) [0,0,2,9,8,10,1,10] :: Array U DIM1 Int
    let testsize = 10000
    let test = fromList (Z :. (testsize::Int)) (take testsize (randoms (mkStdGen 211570155)) :: [Int]) :: Array U DIM1 Int
    print(buySell ins )
    defaultMain[bench "buySell" (nf buySell test)]

