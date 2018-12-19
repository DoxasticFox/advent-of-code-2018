import System.IO.Unsafe
import qualified Data.HashMap.Lazy as HashMap

open fileName = lines $ unsafePerformIO $ readFile fileName

charToCount :: String -> HashMap.HashMap Char Integer
charToCount "" =
    HashMap.empty
charToCount (s:ss) =
    let ssCount = charToCount ss
    in
        if HashMap.member s ssCount
        then
            let curVal = (HashMap.!) ssCount s
            in HashMap.insert s (curVal + 1) ssCount
        else
            HashMap.insert s 1 ssCount

countToCharsRec :: [(Char, Integer)] -> HashMap.HashMap Integer [Char]
countToCharsRec [] =
    HashMap.empty
countToCharsRec (charCountPair:charCountPairs) =
    let
        (char, count) = charCountPair
        countToCharsRec_ = countToCharsRec charCountPairs
    in
        if HashMap.member count countToCharsRec_
        then
            let curVal = (HashMap.!) countToCharsRec_ count
            in HashMap.insert count (char:curVal) countToCharsRec_
        else
            HashMap.insert count [char] countToCharsRec_

countToChars :: String -> HashMap.HashMap Integer [Char]
countToChars =
    countToCharsRec . HashMap.toList . charToCount

checksum :: [String] -> Integer
checksum strings =
    let
        countToChars_ = map countToChars strings
        num2s = length $ filter (HashMap.member 2) countToChars_
        num3s = length $ filter (HashMap.member 3) countToChars_
    in
        toInteger (num2s * num3s)

main = do
    print $ checksum $ open "input"
