{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( runApp,
      Game
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Except (throwE)
import           Core
import           Data.Aeson

import qualified Data.Aeson.Parser
import           Data.Aeson.Types

import           Data.Attoparsec.ByteString
import           Data.ByteString (ByteString)
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Data.Char
import           Data.Time
import           Data.Time.Calendar
import           Data.UUID
import           Database.Redis (ConnectInfo (connectHost, connectPort), defaultConnectInfo, PortID (PortNumber))
import           GHC.Generics
import           Lucid
import           Network.HTTP.Media ((//), (/:))
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Prelude ()
import           Prelude.Compat
import           Redis
import           Servant
import           Servant.Types.SourceT (source)
import           System.Directory
import           Text.Blaze
import qualified Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Utf8
import File
import Network.Wai.Middleware.Cors
import System.Environment (lookupEnv)

type GameApi =
    "startGame" :> Capture "lang" String :> Capture "session" UUID :> Get '[JSON] GameResultModel
    :<|> "check" :> Capture "lang" String :> Capture "session" UUID :> Capture "word" String :> Get '[JSON] GameResultModel


data GameResultModel = GameResultModel
  {
    gid :: UUID
  , gstatus :: String
  , gcomplexity :: Int
  , gcorrectWord :: String
  , grounds :: [(String, String)]
  } deriving (Generic, Show, Read)



instance ToJSON GameResultModel
instance ToJSON CheckResult
instance ToJSON CharWithState
instance ToJSON CharPresense
instance ToJSON Game
instance ToJSON Round

instance ToJSON GameResult
instance ToJSON GameStatus
instance ToJSON FinishedGameResult

startGameHandler :: String -> UUID -> Handler GameResultModel
startGameHandler lng uid = do
  redisHostString <- liftIO $ lookupEnv "REDIS_HOST"
  redisPortString <- liftIO $ lookupEnv "REDIS_PORT"
  let redisConnectInfo = buildConnectInfo redisHostString redisPortString
  maybeCachedGame <- liftIO $ fetchGameRedis redisConnectInfo (gameKey lng uid)
  case maybeCachedGame of
    Just game | lang game == lng -> return $ toGameResultModel $ toGameResult game
    -- Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find user with that ID" })
    _ -> do
       w <- liftIO $ getRandomNoun lng 4
       let game = newGame lng uid w
       liftIO $ cacheGame redisConnectInfo (gameKey lng uid) game
       return $ toGameResultModel $ toGameResult game

checkGameHandler :: String -> UUID -> String -> Handler GameResultModel
checkGameHandler lng uid input = do
  redisHostString <- liftIO $ lookupEnv "REDIS_HOST"
  redisPortString <- liftIO $ lookupEnv "REDIS_PORT"
  let redisConnectInfo = buildConnectInfo redisHostString redisPortString
  maybeCachedGame <- liftIO $ fetchGameRedis redisConnectInfo (gameKey lng uid)
  case maybeCachedGame of
    Just game -> do
      let gameStatus = getGameStatus $ rounds game
      let isExpectedLength = length input == length (word game)
      isKnownWord <- liftIO $ isKnownWord (lang game) input
      let isValid = isKnownWord && isExpectedLength
      let nextGame = nextRound game input
      let isGameFinished = gameIsFinished gameStatus
      unless ( isGameFinished || not isValid) (liftIO $ cacheGame redisConnectInfo (gameKey lng uid) nextGame)
      let resultGame = toGameResultModel $ toGameResult $ if not isGameFinished then nextGame else game
      if not isValid
        then Handler $ throwE err400
        else return resultGame

    Nothing -> Handler $ throwE $ err404 { errBody = "Could not find game with that ID" }

gameKey :: String -> UUID -> String
gameKey lng uid = lng ++ show uid

toGameResultModel :: GameResult -> GameResultModel
toGameResultModel g = GameResultModel (gameId g) (show $ gameStatus g) (complexity g) (correctWord g) (toStringRounds . playedRounds $ g)

toStringRounds :: [Round] -> [(String, String)]
toStringRounds = fmap toStringRound

toStringRound :: Round -> (String, String)
toStringRound r = let (i, w, cr) = unRound r
  in (w, checkResultToString $ unCheckResult cr)

checkResultToString :: [CharWithState] -> String
checkResultToString = fmap charWithStatetoChar

charWithStatetoChar :: CharWithState -> Char
charWithStatetoChar cws | getState cws == WrongPosition = toLower $ character cws
charWithStatetoChar cws | getState cws == RightPosition = toUpper $ character cws
charWithStatetoChar cws  = '#'

buildConnectInfo :: Maybe String -> Maybe String -> ConnectInfo
buildConnectInfo (Just host) (Just port) = 
  defaultConnectInfo 
  { 
    connectHost = host,
    connectPort = PortNumber $ read port 
  }
buildConnectInfo _ _ = 
  defaultConnectInfo 
  { 
    connectHost =  "localhost",
    connectPort = PortNumber 6379 
  }

server1 :: Server GameApi
server1 = startGameHandler 
     :<|> checkGameHandler

gameAPI :: Proxy GameApi
gameAPI = Proxy

myCors :: Middleware
myCors = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"]
        , corsMethods = "GET" : simpleMethods }

getPortNumber :: Maybe String -> Port -> Port
getPortNumber (Just x) _ = read x
getPortNumber Nothing def = def

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = simpleCors $ serve gameAPI server1

runApp :: IO ()
runApp = do
  portString <- lookupEnv "SERVICE_PORT"
  let port = getPortNumber portString 8081
  run port app1
