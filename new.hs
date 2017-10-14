import Control.Applicative
import Control.Monad
import System.IO
import Data.List
import Data.Char
import System.Random (randomRIO)

data Hangman = Guess [Maybe Char] (Char -> Hangman) | Won [Char]

f :: IO (Maybe Char)
f = getLine >>= (\l -> return $ if (length l) == 0
                                then Nothing
                                else Just $ head l)

singleton :: Char -> Hangman
singleton c = Guess [Just c] singleton

printer :: Maybe Char -> IO()
printer mc = case mc of
              Nothing -> return ()
              Just c -> putStrLn $ show c

hangmanStdio :: Hangman -> IO()
hangmanStdio h = f >>= printer


main :: IO ()
main = hangmanStdio $ Guess [] singleton
