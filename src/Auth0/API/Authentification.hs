{-# LANGUAGE OverloadedStrings #-}

module Auth0.API.Authentification
       ( login
       , logout  
       , passwordlessEmail
       , passwordlessSMS
       , passwordlessTouch
       , userInfo
       , tokenInfo
       , linkAccounts
       , unlinkAccounts  
       ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Control.Lens ((^.), (.~), (&))
import           Control.Monad
import           Control.Monad.Except (ExceptT, liftIO, throwError)
import qualified Data.Aeson as AE
import           Data.Aeson (ToJSON, FromJSON, (.=), (.:), (.:?))
import           Auth0.API.Types
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Monoid ((<>))
import           Network.Wreq

--------------------------------------------------------------------------------

-- | Login with username\/password combination
--   in case of SMS login it's phone-number\/access_token  
login :: T.Text -> T.Text -> Config -> ExceptT T.Text IO AccessToken
login user pass Config{..}= do
  resp <- liftIO $ post (T.unpack $ basePath <> "/oauth/ro") requestParams
  case AE.eitherDecode (resp ^. responseBody) of
    (Left err)  -> throwError $ T.pack $ show err
    (Right tok) -> return tok
  where
    requestParams =
      [ "client_id"     := clientId
      , "connection"    := ("sms" :: T.Text)
      , "grant_type"    := ("password" :: T.Text)
      , "username"      := user
      , "password"      := pass
      , "scope"         := ("openid" :: T.Text) -- or "openid name email"  
      ]

logout :: AccessToken -> ExceptT T.Text IO ()
logout token = undefined

--------------------------------------------------------------------------------
-- Passwordless    

-- | Authentification via email
--    
passwordlessEmail :: T.Text -> EmailType -> Config -> ExceptT T.Text IO RespEmail
passwordlessEmail email' type' Config{..} = do
  resp <- liftIO $ post (T.unpack $ basePath <> "/passwordless/start") requestParams
  case AE.eitherDecode (resp ^. responseBody ) of
    (Left err)  -> throwError $ T.pack $ show err
    (Right tok) -> return tok
  where 
    requestParams =
      [ "client_id"  :=  clientId  
      , "connection" :=  ("email" :: T.Text)
      , "email"      :=  email' 
      , "send"       :=  sendType  -- "link" or "code"
      , "authParams" :=  (""::T.Text)   
      ]
    sendType = T.pack $ map toLower $ show type'

-- | Authentification via sms
--      
passwordlessSMS :: T.Text -> Config -> ExceptT T.Text IO RespSMS
passwordlessSMS number Config{..} = do
  resp <- liftIO $ post (T.unpack $ basePath <> "/passwordless/start") requestParams
  case AE.eitherDecode (resp ^. responseBody) of
    (Left err)  -> throwError $ T.pack $ show err
    (Right tok) -> return tok
  where 
    requestParams =
      [ "client_id"    :=  clientId  
      , "connection"   :=  ("sms" :: T.Text)
      , "phone_number" :=  number
      ]
      
passwordlessTouch :: Config -> ExceptT T.Text IO T.Text
passwordlessTouch Config{..} = undefined

--------------------------------------------------------------------------------

-- | Returns the user information based on the Auth0 access token (obtained during login).
--
userInfo :: AccessToken -> Config -> ExceptT T.Text IO UserInfo
userInfo token Config{..} = do
  resp <- liftIO $ getWith opts (T.unpack $ basePath <> "/userinfo") 
  case AE.eitherDecode (resp ^. responseBody) of
    (Left  err) -> throwError $ T.pack $ show err
    (Right ui)  -> return ui
  where
    auth = BS.pack $ T.unpack $ T.concat ["Bearer ", (getToken token)]
    opts = defaults & header "Authorization" .~ [auth]
      
-- | Validates a JSON Web Token (signature and expiration)
-- and returns the user information associated with the user id (sub property) of the token.
tokenInfo :: AccessToken -> Config -> ExceptT T.Text IO UserInfo
tokenInfo token Config{..} = do
  resp <- liftIO $ post (T.unpack $ basePath <> "/tokeninfo") requestParams
  case AE.eitherDecode (resp ^. responseBody) of
    (Left  err) -> throwError $ T.pack $ show err
    (Right ui)  -> return ui
  where
    requestParams =
      [ "id_token" := ((getIdToken token) :: T.Text) 
      ]
    
-- | 
-- 
linkAccounts :: IO ()
linkAccounts = undefined

-- |
-- 
unlinkAccounts :: IO ()
unlinkAccounts = undefined
