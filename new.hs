import Control.Applicative
import Control.Monad
import System.IO
import Data.List
import Data.Char
import System.Random (randomRIO)
import Control.Monad

data Hangman = Guess [Maybe Char] (Char -> Hangman) | Won [Char]

singleton :: Char -> Hangman
singleton c = Guess [Nothing] (\c' -> if c == c' then Won [c] else singleton c)

hangmanStdio :: Hangman -> IO()
hangmanStdio (Won xs) = do
  putStrLn $ "You Won! The winning word was " ++ xs
  
hangmanStdio (Guess xs f) = do
  putStrLn "Try to guess the character:"
  inputLine <- getLine
  hangmanStdio $ f $ head inputLine

main :: IO ()
main = hangmanStdio $ singleton 'c'
