import Control.Applicative
import Control.Monad
import System.IO
import Data.List

data GameBoard = GameBoard { targetWord :: [Char], lettersGuessed :: [Char] }

alphabet = "abcdefghjijklmnopqrstuvwxyz"

sortUniq :: Ord a => [a] -> [a]
sortUniq = nub . sort

takeGuess :: Char -> GameBoard -> GameBoard
takeGuess c gb = (GameBoard (targetWord gb) $ c : (lettersGuessed gb))

processUserAttempt :: Char -> GameBoard -> GameBoard
processUserAttempt c gb = characterAllowed c gb

characterAllowed :: Char -> GameBoard -> Bool
characterAllowed c gb = characterNotPlayedBefore c gb && characterIsInAlphabet c

characterNotPlayedBefore :: Char -> GameBoard -> Bool
characterNotPlayedBefore c gb = c `notElem` (targetWord gb)

characterIsInAlphabet :: Char -> Bool
characterIsInAlphabet c = c `elem` alphabet

-- userWon :: GameBoard -> Bool
-- userWon GameBoard{ targetWord = t, lettersGuessed = l } = -- filter targetWord by lettersGuessed, if greater than 0, False


main :: IO ()
main = do
    t_temp <- getLine
    let c = head t_temp
    putStrLn $ show c
