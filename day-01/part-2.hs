import Data.List
import System.IO.Unsafe

open :: String -> [String]
open fileName = lines $ unsafePerformIO $ readFile fileName

int :: String -> Integer
int = read . dropWhile ('+' ==)

cummulativeSum :: [Integer] -> [Integer]
cummulativeSum xs = 0:(scanl1 (+) xs)

count :: Integer -> [Integer] -> Integer
count needle haystack = toInteger $ length . filter (== needle) $ haystack

prevCount1 :: [Integer] -> Integer
prevCount1 xs = count (last xs) xs

prevCount :: [Integer] -> [Integer]
prevCount xs =
    map prevCount1 $ tail $ inits xs

main = do
    let intStream = map int $ cycle $ open "input"
    let cummulativeSum_ = cummulativeSum intStream
    let sumIntPairs = zip (prevCount cummulativeSum_) cummulativeSum_
    let count2 = filter (\x -> 2 == fst x) sumIntPairs
    print $ snd $ head count2
