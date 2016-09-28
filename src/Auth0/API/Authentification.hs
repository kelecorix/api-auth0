module Auth0.API.Authentification
       ( handshake
       , passwordlessEmail
       , passwordlessSMS
       , passwordlessTouch  
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

handshake :: AccessCode -> Config -> ExceptT Text IO AccessToken
handshake code Config{..} = do
  resp <- liftIO $ post (unpack $ basePath <> "/oauth/token") requestParams
  case AE.eitherDecode (resp ^. responseBody) of
    (Left err)  -> throwError $ pack $ show err
    (Right tok) -> return tok
  where
    requestParams =
      [ "client_id"     := clientId
      , "client_secret" := clientSecret
      , "redirect_uri"  := redirectURI
      , "grant_type"    := grantType
      , "code"          := code
      ]


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
