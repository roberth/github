module GitHub.App
  ( PrivateKey
    -- * Loading the private key
  , loadPrivateKey
  , parsePrivateKey
    -- * Creating a JWT
  , makeToken
  ) where

import qualified OpenSSL.PEM as OpenSSL
import qualified OpenSSL.EVP.PKey as OpenSSL
import qualified OpenSSL.RSA as OpenSSL

import qualified Crypto.PubKey.RSA.Types as RSA
import           Crypto.PubKey.RSA.Types ( PrivateKey )

import qualified Crypto.JWT as JWT
import qualified Crypto.JOSE.JWK as JWK
import qualified Crypto.JOSE.JWS as JWS
import qualified Crypto.JOSE.Compact as Compact

import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString (ByteString)

import Data.Maybe
import Data.String
import Data.Time
import Control.Exception
import Control.Lens
import Control.Monad.Except

loadPrivateKey :: FilePath -> IO PrivateKey
loadPrivateKey path = do
  keyString <- readFile path
  parsePrivateKey keyString

parsePrivateKey :: String -> IO PrivateKey
parsePrivateKey keyString = do
  key <- OpenSSL.readPrivateKey keyString OpenSSL.PwNone
  case convert key of
    Left e -> throw (userError $ "Failed to load GitHub client private key: " ++ e)
    Right r -> pure r

convert :: OpenSSL.SomeKeyPair -> Either String RSA.PrivateKey
convert key = convert' <$> OpenSSL.toKeyPair key `orLeft` "Not an RSA keypair"

convert' :: OpenSSL.RSAKeyPair -> RSA.PrivateKey
convert' kp = RSA.PrivateKey
  { RSA.private_pub = RSA.PublicKey
    { RSA.public_size = OpenSSL.rsaSize kp
    , RSA.public_n = OpenSSL.rsaN kp
    , RSA.public_e = OpenSSL.rsaE kp
    }
  , RSA.private_d = OpenSSL.rsaD kp
  , RSA.private_p = OpenSSL.rsaP kp
  , RSA.private_q = OpenSSL.rsaQ kp
  , RSA.private_dP = fromMaybe 0 $ OpenSSL.rsaDMP1 kp
  , RSA.private_dQ = fromMaybe 0 $ OpenSSL.rsaDMQ1 kp
  , RSA.private_qinv = fromMaybe 0 $ OpenSSL.rsaIQMP kp
  }

orLeft :: Maybe a -> b -> Either b a
orLeft (Just x) _ = Right x
orLeft Nothing  l = Left l

floorTime :: UTCTime -> UTCTime
floorTime t = t { utctDayTime = fromIntegral (floor $ toRational $ utctDayTime t :: Int) }

makeToken :: PrivateKey -> IO ByteString
makeToken privateKey = do
  now <- floorTime <$> getCurrentTime
  let nowSeconds = JWT.NumericDate now
      expSeconds = JWT.NumericDate ((60 * 10 - 2) `addUTCTime` now)
        -- minus one as an error margin for leap seconds (if recognized by the server)
        -- minus one again for off by one error

      jwk = JWK.fromRSA privateKey
      jwsHeader = JWS.newJWSHeader ((), JWS.RS256)
      claimsSet = JWT.emptyClaimsSet
                          -- FIXME: read from environment
                   & JWT.claimIss .~ Just (fromString "6881") -- See
                       -- https://github.com/settings/apps/lambda-ci-local-instance
                       -- "About" / "ID:"
                   & JWT.claimIat .~ Just nowSeconds
                   & JWT.claimExp .~ Just expSeconds

  result <- runExceptT $ do
    jwt <- JWT.signClaims jwk jwsHeader claimsSet
    pure $ Compact.encodeCompact jwt

  case result :: Either JWT.JWTError LBS.ByteString of
    Left e -> throw (userError (show e))
    Right lbs -> pure (lbs ^. strict)
