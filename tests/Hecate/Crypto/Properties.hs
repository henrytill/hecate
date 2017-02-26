module Hecate.Crypto.Properties (cryptoTests) where

import Control.Monad (replicateM)
import Crypto.Error (CryptoFailable (..))
import Data.Text.Encoding
import Hecate.Crypto
import Hecate.Orphans ()
import Test.QuickCheck
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T

roundTrip
  :: T.Text
  -> BS.ByteString
  -> BS.ByteString
  -> BS.ByteString
  -> T.Text
  -> Bool
roundTrip password salt nonce aad text =
  let key               = generateKey password salt
      encodedText       = encodeUtf8 text
      (encrypted, etag) = case encrypt nonce key aad encodedText of
                            CryptoFailed e      -> error (show e)
                            CryptoPassed (c, t) -> (Base64.encode c, t)
      (decrypted, dtag) = case Base64.decode encrypted of
                            Left e   -> error (show e)
                            Right bs -> case decrypt nonce key aad bs of
                              CryptoFailed e      -> error (show e)
                              CryptoPassed (d, t) -> (decodeUtf8 d, t)
  in decrypted == text && dtag == etag

prop_roundTrip :: Property
prop_roundTrip =
  forAll arbitrary $ \password ->
  forAll arbitrary $ \salt     ->
  forAll arbNonce  $ \nonce    ->
  forAll arbitrary $ \aad      ->
  forAll arbitrary $ \text     ->
  roundTrip password salt nonce aad text
  where
    arbNonce = C.pack <$> replicateM 12 arbitrary

cryptoTests :: [Property]
cryptoTests = [prop_roundTrip]