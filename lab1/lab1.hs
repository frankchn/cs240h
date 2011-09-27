import qualified Data.Map as Map
import System.Environment
import Data.Char as Char

split :: String -> [String]
split [] = [""]
split (x:xs) | not (Char.isAlpha x) = "" : rest
   			 | otherwise = (x : head rest) : tail rest
   			   where rest = split xs
       
countStr :: [String] -> Map.Map String Int -> Map.Map String Int
countStr [] _ = Map.empty
countStr ("":xs) m = countStr xs m
countStr (x:xs) m = Map.insertWith' (+) (map toLower x) 1 (countStr xs m)

genStars :: Int -> String
genStars n = replicate n '*'

collapse :: String -> String -> Int -> String 
collapse result k a = result ++ k ++ " " ++ genStars a ++ "\n"

genHistogram :: String -> String
genHistogram s = Map.foldlWithKey collapse "" (countStr (split s) Map.empty)

main = do
  args <- getArgs
  mapM_ (interactFile genHistogram) args
 
interactFile f fileName = do
  s <- readFile fileName
  putStr (f s)
