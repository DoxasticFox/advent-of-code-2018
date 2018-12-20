import System.IO.Unsafe
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Set as Set

open fileName = lines $ unsafePerformIO $ readFile fileName

stringToCount :: [String] -> HashMap.HashMap String Integer
stringToCount [] =
    HashMap.empty
stringToCount (s:ss) =
    let ssCount = stringToCount ss
    in
        if HashMap.member s ssCount
        then
            let curVal = (HashMap.!) ssCount s
            in HashMap.insert s (curVal + 1) ssCount
        else
            HashMap.insert s 1 ssCount

deleteNth :: Integer -> String -> String
deleteNth n string =
    snd $ unzip $ filter (\x -> n /= fst x) $ zip [0..] string

deleteNthInList :: Integer -> [String] -> [String]
deleteNthInList n strings =
    map (deleteNth n) strings

waysToDeleteNthInList :: [String] -> [[String]]
waysToDeleteNthInList strings =
    let
        maxStringLength = toInteger $ maximum $ map length strings
    in
        map (\n -> deleteNthInList n strings) [0..maxStringLength]

matchingString strings =
    let
        stringCounts = map stringToCount $ waysToDeleteNthInList strings
        matchingStrCounts = map (HashMap.filter (2 ==)) stringCounts
        matchingStrCount = filter (\x -> 1 == HashMap.size x) matchingStrCounts
    in
        fst $ head $ HashMap.toList $ head matchingStrCount

main = do
    print $ matchingString $ open "input"
