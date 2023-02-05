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

cacheGame :: ConnectInfo -> UUID -> Game -> IO ()
cacheGame redisInfo uid game = runRedisAction redisInfo $ void $ 
  setex (pack . show $ uid) 3600 (pack . show $ game)

fetchGameRedis :: ConnectInfo -> UUID -> IO (Maybe Game)
fetchGameRedis redisInfo uid = runRedisAction redisInfo $ do
  result <- get (pack . show $ uid)
  case result of
    Right (Just gameString) -> return $ Just (read . unpack $ gameString)
    _ -> return Nothing

deleteGameCache :: ConnectInfo -> UUID -> IO ()
deleteGameCache redisInfo uid = do
  connection <- connect redisInfo
  runRedis connection $ do
    _ <- del [pack . show $ uid]
    return ()