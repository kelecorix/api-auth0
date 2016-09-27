module Auth0.API.Internal.Access where

import Network.HTTP.Dispatch.Core
import qualified Aeson as AE

--------------------------------------------------------------------------------

handshake :: AccessCode -> Config -> ExceptT Text IO AccessToken
handshake code Config{..} = do
  resp <- liftIO $ runRequest (post (unpack $ basePath <> "/oauth/token"))
  case AE.eitherDecode (respBody resp) of
    (Left err)  -> throwError $ pack $ show err
    (Right tok) -> return tok
  where
    requestParams =
      [ "client_id"     := getClientID
      , "client_secret" := getClientSecret
      , "redirect_uri"  := getRedirectURI
      , "grant_type"    := getGrantType
      , "code"          := code
      ]
