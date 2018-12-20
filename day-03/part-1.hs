import System.IO.Unsafe

open fileName = lines $ unsafePerformIO $ readFile fileName
