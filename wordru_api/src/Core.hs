{-# LANGUAGE DeriveGeneric #-}

module Core where
import GHC.Generics
import Data.UUID
import Data.Char


data CharPresense = Missing | WrongPosition | RightPosition
  deriving (Generic, Show, Read, Eq)

data FinishedGameResult = Win | Lose
  deriving (Generic, Show, Read, Eq)

data GameStatus = New | InProgress | Finished FinishedGameResult
  deriving (Generic, Show, Read, Eq)

data CharWithState = CharWithState
  {
    character :: Char
  , getState :: CharPresense
  } deriving (Generic, Show, Read)


newtype CheckResult = CheckResult
  {
    unCheckResult :: [CharWithState]
  } deriving (Generic, Show, Read)

data Game = Game
  {
    lang :: String
  , id :: UUID
  , word :: String
  , rounds :: [Round]
  } deriving (Generic, Show, Read)

-- Get rid of enum and make it as SUM type with OR
data GameResult = GameResult
  {
    gameId :: UUID
  , gameStatus :: GameStatus
  , complexity :: Int
  , correctWord :: String
  , playedRounds :: [Round]
  } deriving (Generic, Show, Read)

newtype Round = Round
  {
    unRound :: (Int, String, CheckResult)
  } deriving (Generic, Show, Read)

gameIsFinished :: GameStatus -> Bool
gameIsFinished (Finished _) = True
gameIsFinished _ = False

newGame :: String -> UUID -> String ->Game
newGame lang uid word = Game lang uid word []

nextRound :: Game -> String -> Game
nextRound (Game lang id cw r) inputWord =
  let roundResult = calculateResult cw inputWord
      rounds = Round (length r, inputWord, roundResult):r
  in
  Game lang id cw rounds

calculateResult :: String -> String -> CheckResult
calculateResult w s = CheckResult $ validateCharPair (zip  w s) w

validateCharPair :: [(Char, Char)] -> String -> [CharWithState]
validateCharPair [] w = []
validateCharPair ((e, a):xs) w = CharWithState a (getStatus e a w) : validateCharPair xs w

getStatus :: Char -> Char -> String -> CharPresense
getStatus e a w
  | toLower e == toLower a = RightPosition
  | toLower a `elem` fmap toLower w = WrongPosition
  | otherwise  = Missing

toGameResult :: Game -> GameResult
toGameResult (Game lang i w rs) = GameResult i (getGameStatus rs) (length w) w rs

getGameStatus :: [Round] -> GameStatus
getGameStatus [] = New
getGameStatus rx
  | allLettersMatch rx = Finished Win
  | length  rx < 6 = InProgress
  | otherwise = Finished Lose

allLettersMatch :: [Round] -> Bool
allLettersMatch [] = False
allLettersMatch rx = any allLettersMatchForRound rx

allLettersMatchForRound :: Round -> Bool
allLettersMatchForRound r = allLettersMatchForCheckResult $ unRound r

allLettersMatchForCheckResult :: (Int, String, CheckResult) -> Bool
allLettersMatchForCheckResult (_,_,c) = all allLettersMatchForChars (unCheckResult c)

allLettersMatchForChars :: CharWithState -> Bool
allLettersMatchForChars c = getState c == RightPosition

stringEqualCaseInsensitive :: String -> String -> Bool
stringEqualCaseInsensitive s1 s2 = fmap toLower s1 == fmap toLower s2

wordHasOnlyLetters :: String -> Bool
wordHasOnlyLetters [] = False
wordHasOnlyLetters [x] = isLetter x
wordHasOnlyLetters (x:xs) = wordHasOnlyLetters xs && isLetter x