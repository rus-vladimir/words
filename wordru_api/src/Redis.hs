module Redis where
import           Database.Redis
import           Data.UUID
import           Core
import           Data.Int
import           Control.Monad (void)
import           Data.ByteString.Char8 (pack, unpack)

fetchRedisConnection :: IO ConnectInfo
fetchRedisConnection = return defaultConnectInfo

runRedisAction :: ConnectInfo -> Redis a -> IO a
runRedisAction redisInfo action = do
  connection <- connect redisInfo
  runRedis connection action

cacheGame :: ConnectInfo -> String -> Game -> IO ()
cacheGame redisInfo key game = runRedisAction redisInfo $ void $ 
  setex (pack key) 3600 (pack . show $ game)

fetchGameRedis :: ConnectInfo -> String -> IO (Maybe Game)
fetchGameRedis redisInfo key = runRedisAction redisInfo $ do
  result <- get (pack key)
  case result of
    Right (Just gameString) -> return $ Just (read . unpack $ gameString)
    _ -> return Nothing

deleteGameCache :: ConnectInfo -> String -> IO ()
deleteGameCache redisInfo key = do
  connection <- connect redisInfo
  runRedis connection $ do
    _ <- del [pack key]
    return ()