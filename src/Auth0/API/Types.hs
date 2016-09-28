{-# LANGUAGE DeriveAnyClass #-}

module Auth0.API.Types
  ( AccessToken(..)
  , AccessCode(..)
  , Identity(..)
  , User(..)
  , Config(..)
  , RespEmail(..)
  , RespSMS(..)
  , EmailType(..)  
  ) where

import           Control.Monad
import qualified Data.Aeson as AE
import           Data.Aeson (ToJSON, FromJSON, (.=), (.:), (.:?))
import           Data.Monoid ((<>))
import           Data.Text (Text, pack, unpack)
import           Data.Text.Encoding (encodeUtf8)
import           GHC.Generics

--------------------------------------------------------------------------------

data Config = Config
  { clientId       :: Text
  , clientSecret   :: Text
  , redirectURI    :: Text
  , grantType      :: Text -- TODO: sum type
  , basePath       :: Text
  , apiAccessToken :: Text
} deriving (Show, Eq)

type AccessCode = Text

data AccessToken = AccessToken
  { getToken     :: Text
  , getIdToken   :: Text
  , getTokenType :: Text -- TODO: sum type
  } deriving (Show, Eq, Generic)

instance ToJSON AccessToken where
  toJSON (AccessToken tok tokId tokType) =
    AE.object
    [ "access_token" .= tok
    , "id_token"     .= tokId
    , "token_type"   .= tokType
    ]

instance FromJSON AccessToken where
  parseJSON (AE.Object v) =
    AccessToken
      <$> v .: "access_token"
      <*> v .: "id_token"
      <*> v .: "token_type"
  parseJSON _ = mzero

data Identity = Identity
  { userId      :: Text
  , provider    :: Text -- TODO: sum type
  , accessToken :: Maybe Text
  , connection  :: Text -- TODO: sum type
  , isSocial    :: Bool
  } deriving (Show, Eq)  -- don't use Generic

instance ToJSON Identity where
  toJSON Identity{..} =
    AE.object
    [ "user_id"      .= userId
    , "provider"     .= provider
    , "access_token" .= accessToken
    , "connection"   .= connection
    , "is_social"    .= isSocial
    ]

-- | Some providers send a 'user_id' as an Int while others use a String
--  thus specific handler 
instance FromJSON Identity where
  parseJSON = AE.withObject "identity" $ \v -> do
    uid    <- v .:  "user_id" >>= parseId
    prov   <- v .:  "provider"
    tok    <- v .:? "access_token"
    conn   <- v .:  "connection"
    social <- v .:  "isSocial"
    return $ Identity uid prov tok conn social
    where
      parseId (AE.String strId) = return strId
      parseId (AE.Number numId) = return . pack . show $ numId
      parseId _ = mzero

data User =
  User { uid        :: Text
       , email         :: Text
       , emailVerified :: Bool
       , picture       :: Text 
       , name          :: Text
       , nickname      :: Text
       , identities    :: [Identity]
       } deriving (Show, Eq)

instance ToJSON User where
  toJSON User{..} =
    AE.object
    [ "user_id"        .= uid
    , "email"          .= email
    , "email_verified" .= emailVerified
    , "picture"        .= picture
    , "name"           .= name
    , "nickname"       .= nickname
    , "identities"     .= identities
    ]

instance FromJSON User where
  parseJSON (AE.Object v) =
    User
      <$> v .: "user_id"
      <*> v .: "email"
      <*> v .: "email_verified"
      <*> v .: "picture"
      <*> v .: "name"
      <*> v .: "nickname"
      <*> v .: "identities"
  parseJSON _ = mzero

-- {
--  "_id": "57eba696469d42056d4abdee",
--  "email": "sergey.bushnyak@sigrlami.eu"
--}
data RespEmail =
  RespEmail { re_id :: Text
            , re_email :: Text
            } deriving (Show, Eq, Generic, FromJSON)

--  {
--   "_id": "57eba5b1469d42056d4abdea",
--  "phone_number": "+380937451652",
--  "request_language": "en-US,en;q=0.5"
-- }    
data RespSMS = 
  RespSMS { rs_id            :: Text
          , rs_phone_number     :: Text
          , rs_request_language :: Text
          } deriving (Show, Eq, Generic, FromJSON)

data EmailType = Link
               | Code
               deriving (Show)
