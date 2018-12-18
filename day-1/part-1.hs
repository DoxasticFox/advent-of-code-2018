import System.IO.Unsafe

open fileName = lines $ unsafePerformIO $ readFile fileName

int = read . dropWhile ('+' ==)

main = print $ sum $ map int $ open "input"
