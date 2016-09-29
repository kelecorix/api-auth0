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

import           Data.Char
import           Control.Lens ((^.))
import           Control.Monad
import           Control.Monad.Except (ExceptT, liftIO, throwError)
import qualified Data.Aeson as AE
import           Data.Aeson (ToJSON, FromJSON, (.=), (.:), (.:?))
import           Auth0.API.Types
import           Data.Text (Text, pack, unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Monoid ((<>))
import           Network.Wreq

--------------------------------------------------------------------------------

-- | Login with username\/password combination
--   in case of SMS login it's phone-number\/access_token  
login :: Text -> Text -> Config -> ExceptT Text AccessToken
login user pass Config{..}= do
  resp <- liftIO $ post (unpack $ basePath <> "/oauth/ro") requestParams
  case AE.eitherDecode (resp ^. responseBody) of
    (Left err)  -> throwError $ pack $ show err
    (Right tok) -> return tok
  where
    requestParams =
      [ "client_id"     := clientId
      , "connection"    := "sms"
      , "grant_type"    := "password"
      , "username"      := user
      , "password"      := pass
      , "scope"         := "openid" -- or "openid name emaol"  
      ]

logout :: AccessToken -> ExceptT Text IO ()
logout token = undefined

--------------------------------------------------------------------------------
-- Passwordless    

-- | Authentification via email
--    
passwordlessEmail :: Text -> EmailType -> Config -> ExceptT Text IO RespEmail
passwordlessEmail email' type' Config{..} = do
  resp <- liftIO $ post (unpack $ basePath <> "/passwordless/start") requestParams
  case AE.eitherDecode (resp ^. responseBody ) of
    (Left err)  -> throwError $ pack $ show err
    (Right tok) -> return tok
  where 
    requestParams =
      [ "client_id"  :=  clientId  
      , "connection" :=  ("email" :: Text)
      , "email"      :=  email' 
      , "send"       :=  sendType  -- "link" or "code"
      , "authParams" :=  (""::Text)   
      ]
    sendType = pack $ map toLower $ show type'

-- | Authentification via sms
--      
passwordlessSMS :: Text -> Config -> ExceptT Text IO RespSMS
passwordlessSMS number Config{..} = do
  resp <- liftIO $ post (unpack $ basePath <> "/passwordless/start") requestParams
  case AE.eitherDecode (resp ^. responseBody) of
    (Left err)  -> throwError $ pack $ show err
    (Right tok) -> return tok
  where 
    requestParams =
      [ "client_id"    :=  clientId  
      , "connection"   :=  ("sms" :: Text)
      , "phone_number" :=  number
      ]
      
passwordlessTouch :: Config -> ExceptT Text IO Text
passwordlessTouch Config{..} = undefined


--------------------------------------------------------------------------------

-- | Returns the user information based on the Auth0 access token (obtained during login).
--
userInfo :: AccessToken -> ExceptT Text IO UserInfo
userInfo token = do
 -- supplu Autho access_token
  resp <- liftIO $ getWith opts (unpack $ basePath <> "/userinfo") 
  case AE.eitherDecode (resp ^. responseBody) of
    (Left err)  -> throwError $ pack $ show err
    (Right tok) -> return tok
  where 
    opts = defaults & header "Authorization" .~ ("Bearer " ++ (access_token token))
      
-- | Validates a JSON Web Token (signature and expiration)
-- and returns the user information associated with the user id (sub property) of the token.
tokenInfo :: AccessToken -> ExceptT Text IO UsernInfo
tokenInfo token = do
 --supplu id_token
  resp <- liftIO $ post (unpack $ basePath <> "/tokeninfo") requestParams
  case AE.eitherDecode (resp ^. responseBody) of
    (Left err)  -> throwError $ pack $ show err
    (Right tok) -> return tok
  where 
    requestParams =
      [ "id_token" := (id_token token)  
      ]

-- | 
-- 
linkAccounts :: IO ()
linkAccounts = undefined

-- |
-- 
unlinkAccounts :: IO ()
unlinkAccounts = undefined
