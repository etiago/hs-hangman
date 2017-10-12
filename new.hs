import Control.Applicative
import Control.Monad
import System.IO
import Data.List
import Data.Char
import System.Random (randomRIO)

data Hangman = Guess [Maybe Char] (Char -> Hangman) | Won [Char]

singleton :: Char -> Hangman
singleton c = c

hangmanStdio :: Hangman -> IO()
hangmanStdio h = do
  putStrLn "Guess the character"
  l <- getLine
  let c = toLower $ head l
  if h == c
  then do
    putStrLn "Won"
    return ()
  else do
    putStrLn "Wrong"
    hangmanStdio h

main :: IO ()
main = hangmanStdio 'b'
