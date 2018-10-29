{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import           Data.Map.Strict (fromList, lookup)
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.Text.Lazy as TL
import           Dhall
import           Network.HTTP.Types (status401)
import           Network.Mail.Mime
import           Network.Wai.Middleware.HttpAuth
import           Web.JWT
import           Web.Scotty

data Cfg = Cfg { cfgKey :: T.Text, cfgUser :: User } deriving (Show, Generic);
data User = User {
  userUid :: T.Text
  , userUsername :: T.Text
  , userMail :: T.Text } deriving (Show, Generic)

instance Interpret User
instance Interpret Cfg

jswt2user :: Web.JWT.JSON -> Maybe User
jswt2user jswt = mbuser (lookclaims jswt "uid") (lookclaims jswt "username") (lookclaims jswt "mail")
  where
    lookclaims :: Web.JWT.JSON -> T.Text -> Maybe T.Text
    lookclaims token key = case fmap (Data.Map.Strict.lookup key) $ uclaims token of
      Just r -> Data.Aeson.decode $ encode r
      Nothing -> Nothing
    mbuser :: (Maybe T.Text) -> (Maybe T.Text) -> (Maybe T.Text) -> Maybe User
    mbuser Nothing _ _ = Nothing
    mbuser _ Nothing _ = Nothing
    mbuser _ _ Nothing = Nothing
    mbuser (Just u) (Just un) (Just m) = Just $ User u un m

uclaims :: JSON -> Maybe ClaimsMap
uclaims = fmap (unregisteredClaims . claims) . Web.JWT.decode

main :: IO()
main = do

  putStrLn "Starting ..."

  dbu <- input auto "./config.dhall"
  let cfg = (dbu :: Cfg)
  scotty 3000 $ do
    post "/auth/:key/" $ do
      ik <- param "key"
      if (cfgKey cfg /= ik)
        then
        status status401
        else do
        let cs = def { unregisteredClaims = fromList [ ("mail", (String $ userMail $ cfgUser cfg))
                                                     , ("uid", (String $ userUid $ cfgUser cfg))
                                                     , ("username", (String $ userUsername $ cfgUser cfg)) ] }
        let key = secret $ cfgKey (dbu::Cfg)
        Web.Scotty.json $ encodeSigned HS256 key cs

    post "/send/" $ do
      msg <- param "message"
      auth <- Web.Scotty.header "Authorization"
      case (fmap fromJust) $ fmap (extractBearerAuth . encodeUtf8 . TL.toStrict) auth of
            Just t -> case jswt2user (decodeUtf8 t) of
              Just u -> do
                liftAndCatchIO $ return (simpleMail'
                                         (Address (Just (userUsername u)) (userMail u))
                                         (Address (Just "SMail") "noreply@px.io")
                                         "mail send from smailws" (TL.pack msg)) >>= renderSendMail
                html $ "Send mail"
              Nothing -> status status401
            Nothing -> status status401

    get "/" $ do
      liftAndCatchIO $ putStrLn "get /"
      html "smailws api"
