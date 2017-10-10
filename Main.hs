import Control.Applicative
import Control.Monad
import System.IO
import Data.List
import Data.Char

data GameBoard = GameBoard { targetWord :: [Char], lettersGuessed :: [Char], lettersLeft :: [Char] }
  deriving (Show)
data UserAttemptOutcome = UserWon [Char] | CharacterAlreadyPlayed [Char] | CharacterNotInAlphabet [Char]
  deriving (Show)

alphabet = "abcdefghjijklmnopqrstuvwxyz"

sortUniq :: Ord a => [a] -> [a]
sortUniq = nub . sort

takeGuess :: Char -> GameBoard -> GameBoard
takeGuess c gb = (GameBoard
                  (targetWord gb)
                  (c : (lettersGuessed gb))
                  $ filter (`notElem` [c]) (lettersLeft gb))

passthrough :: GameBoard -> GameBoard
passthrough gb = gb

processUserAttempt :: Char -> GameBoard -> Either UserAttemptOutcome GameBoard
processUserAttempt c gb = do
                          let characterAllowed = isCharacterAllowed c gb
                          if  characterAllowed
                          then do
                            let newGameBoard = (takeGuess c gb)
                            if hasUserWon newGameBoard
                            then Left (UserWon "Congrats!")
                            else Right newGameBoard
                          else Left (CharacterAlreadyPlayed "This character's already been played")

isCharacterAllowed :: Char -> GameBoard -> Maybe UserAttemptOutcome
isCharacterAllowed c gb = if not characterNotPlayedBefore c gb
                          then Just (CharacterAlreadyPlayed "This character's already been played")
                          else if not characterIsInAlphabet c
                            then Just (CharacterNotInAlphabet "This character's not in the alphabet")
                            else Nothing

characterNotPlayedBefore :: Char -> GameBoard -> Bool
characterNotPlayedBefore c gb = c `notElem` (lettersGuessed gb)

characterIsInAlphabet :: Char -> Bool
characterIsInAlphabet c = c `elem` alphabet

shouldQuitGame :: String -> Bool
shouldQuitGame s = s == "quit"

hasUserWon :: GameBoard -> Bool
hasUserWon gb = True
-- hasUserWon GameBoard{ targetWord = t, lettersGuessed = l } = -- filter targetWord by lettersGuessed, if greater than 0, False

gameLoop :: GameBoard -> IO ()
gameLoop gb = do
    putStrLn "Please input a letter:"
    t_temp <- getLine
    if not $ shouldQuitGame t_temp then
      do
        let c = toLower $ head t_temp
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

prepareGameBoard :: [Char] -> GameBoard
prepareGameBoard word = do
                          let lowerCaseWord = fmap toLower word
                          (GameBoard lowerCaseWord [] (sortUniq lowerCaseWord))

main :: IO ()
main = gameLoop $ prepareGameBoard "Tiago"
