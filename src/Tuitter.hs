{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
import Web.Authenticate.OAuth
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit

{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString
import Data.Text.Encoding (encodeUtf8)

import Data.ByteString.Char8
import Codec.Binary.UTF8.String

import Data.Char
import System.IO

import System.Console.Haskeline
import Data.Maybe

myOAuth :: OAuth
myOAuth = newOAuth { oauthServerName = "api.twitter.com"
                   , oauthConsumerKey = "LWTAypKV0UPkOcN7nehd6Guix"
                   , oauthConsumerSecret = "WBOH05OAsByPeoeDpESWcuw66RRyPlHOhBF79qbybSG5r0cl0k"
                   }

myCred :: Credential
myCred = newCredential "791270600903757824-f8FPgOrjJIb5nCB3gdq23mTLMesgXKb"
                       "GmOZuh4xabqRJEoPJM1qptBTl1oqfbyFeGLoMbh01ZzhU"

data Tweet = Tweet { text :: !Text
                   } deriving (Show, Generic)

instance FromJSON Tweet 
instance ToJSON Tweet

getTweets :: String -> IO (Either String [Tweet])
getTweets name = do
    req <- parseUrl $ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++ name
    m <- newManager tlsManagerSettings
    res <- do
            signedreq <- signOAuth myOAuth myCred req
            httpLbs signedreq m
    return $ eitherDecode $ responseBody res

{-
main :: IO ()
main = do
    ets <- getTweets "ttk_vim"
    case ets of
      Left err -> putStrLn err
      Right ts -> mapM_ T.putStrLn . map text $ take 5 ts
      -}

tweet :: ByteString -> IO ()
tweet tw = do
    req <- parseUrl "https://api.twitter.com/1.1/statuses/update.json"
    let postReq = urlEncodedBody [("status", tw)] req
    m <- newManager tlsManagerSettings
    res <- do
            signedreq <- signOAuth myOAuth myCred postReq
            httpLbs signedreq m
    return ()

tweetRun :: IO ()
tweetRun = do str <- fromMaybe "" <$> (runInputT defaultSettings 
                                   $ getInputLine "Input your tweet> ")
              Data.ByteString.putStr "Tweet?[Y/n]> "
              hSetBuffering stdin NoBuffering
              ans <- getChar
              Data.ByteString.putStrLn ""
              case ans of
                  'y' -> do tweet . Data.ByteString.Char8.pack
                                  $ encodeString str
                            continue
                  'Y' -> do tweet . Data.ByteString.Char8.pack
                                  $ encodeString str
                            continue
                  _   -> continue
              where continue =  do Data.ByteString.putStr "Continue?[Y/n]> "
                                   ans2 <- getChar
                                   Data.ByteString.putStrLn ""
                                   case ans2 of
                                       'y' -> tweetRun
                                       'Y' -> tweetRun
                                       _   -> return ()

main = tweetRun
