import Control.Applicative
import Control.Monad
import System.IO
import Data.List

data GameBoard = GameBoard { targetWord :: [Char], lettersGuessed :: [Char] }
  deriving (Show)
data UserInputError = CharacterAlreadyPlayed [Char] | CharacterNotInAlphabet [Char]
  deriving (Show)

alphabet = "abcdefghjijklmnopqrstuvwxyz"

sortUniq :: Ord a => [a] -> [a]
sortUniq = nub . sort

takeGuess :: Char -> GameBoard -> GameBoard
takeGuess c gb = (GameBoard (targetWord gb) $ c : (lettersGuessed gb))

passthrough :: GameBoard -> GameBoard
passthrough gb = gb

processUserAttempt :: Char -> GameBoard -> Either UserInputError GameBoard
processUserAttempt c gb = if characterAllowed c gb
                          then Right (takeGuess c gb)
                          else Left (CharacterAlreadyPlayed "This character's already been played")

characterAllowed :: Char -> GameBoard -> Bool
characterAllowed c gb = characterNotPlayedBefore c gb
                        && characterIsInAlphabet c

characterNotPlayedBefore :: Char -> GameBoard -> Bool
characterNotPlayedBefore c gb = c `notElem` (lettersGuessed gb)

characterIsInAlphabet :: Char -> Bool
characterIsInAlphabet c = c `elem` alphabet

shouldQuitGame :: String -> Bool
shouldQuitGame s = s == "quit"
-- userWon :: GameBoard -> Bool
-- userWon GameBoard{ targetWord = t, lettersGuessed = l } = -- filter targetWord by lettersGuessed, if greater than 0, False

gameLoop :: GameBoard -> IO ()
gameLoop gb = do
    putStrLn "Please input a letter:"
    t_temp <- getLine
    if not $ shouldQuitGame t_temp then
      do
        let c = head t_temp
        case processUserAttempt c gb of
          Left msg -> do
            putStrLn $ show msg
            putStrLn $ show gb
            gameLoop gb
          Right newGameBoard -> do
            putStrLn $ show newGameBoard
            gameLoop newGameBoard
    else
      return ()

main :: IO ()
main = gameLoop (GameBoard "Tiago" [])
