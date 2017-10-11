import Control.Applicative
import Control.Monad
import System.IO
import Data.List
import Data.Char
import System.Random (randomRIO)

data GameBoard = GameBoard { targetWord :: [Char], lettersGuessed :: [Char], lettersLeft :: [Char] }
  deriving (Show)
data UserAttemptOutcome = CharacterAlreadyPlayed [Char] | CharacterNotInAlphabet [Char]
  deriving (Show)
type ValidChar = Char

randomWords = ["copper", "explain", "truck", "neat", "unite"]
alphabet = "abcdefghjijklmnopqrstuvwxyz"

sortUniq :: Ord a => [a] -> [a]
sortUniq = nub . sort

takeGuess :: ValidChar -> GameBoard -> GameBoard
takeGuess c gb = (GameBoard
                  (targetWord gb)
                  (c : (lettersGuessed gb))
                  $ filter (`notElem` [c]) (lettersLeft gb))

passthrough :: GameBoard -> GameBoard
passthrough gb = gb

isCharacterAllowed :: Char -> GameBoard -> Either UserAttemptOutcome ValidChar
isCharacterAllowed c gb = if not $ characterNotPlayedBefore c gb
                          then Left (CharacterAlreadyPlayed "This character's already been played")
                          else if not $ characterIsInAlphabet c
                          then Left (CharacterNotInAlphabet "This character's not in the alphabet")
                          else Right c

characterNotPlayedBefore :: Char -> GameBoard -> Bool
characterNotPlayedBefore c gb = c `notElem` (lettersGuessed gb)

characterIsInAlphabet :: Char -> Bool
characterIsInAlphabet c = c `elem` alphabet

shouldQuitGame :: String -> Bool
shouldQuitGame s = s == "quit"

hasUserWon :: GameBoard -> Bool
hasUserWon gb = length (lettersLeft gb) == 0

printWinMessage :: GameBoard -> IO ()
printWinMessage gb = do
  putStrLn $ "Congrats! You guessed the word in " ++ show (length (lettersGuessed gb)) ++ " attempts."

gameLoop :: GameBoard -> IO ()
gameLoop gb = do
    putStrLn "Please input a letter:"
    t_temp <- getLine
    if not $ shouldQuitGame t_temp then
      do
        let c = toLower $ head t_temp
        case isCharacterAllowed c gb of
          Left uao -> do
            putStrLn $ show uao
            gameLoop gb
          Right vc -> do
            let newGameBoard = takeGuess c gb
            putStrLn $ show newGameBoard
            if hasUserWon newGameBoard
              then printWinMessage newGameBoard
              else gameLoop newGameBoard
    else
      return ()

prepareGameBoard :: [Char] -> GameBoard
prepareGameBoard word = do
                          let lowerCaseWord = fmap toLower word
                          (GameBoard lowerCaseWord [] (sortUniq lowerCaseWord))

pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

main :: IO ()
main = do
  w <- pick randomWords
  gameLoop $ prepareGameBoard w
