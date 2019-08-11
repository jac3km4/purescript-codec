module Data.Codec
  ( Read(..)
  , decode
  , encode
  , runReadEff
  , runReadEff'
  , genericDecode
  , genericDecode'
  , genericEncode
  , genericEncode'
  , class Decoder
  , class GenericDecoder
  , class Encoder
  , class GenericEncoder
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (foldMap)
import Data.Array as Array
import Data.Codec.Types (Byte, Short)
import Data.ByteString (ByteString, ReadResult(..))
import Data.ByteString as BS
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), from, to)
import Data.Long as Long
import Data.Long.Internal (Signed)
import Data.Long.Internal as LongInt
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (Tuple(..), fst)
import Data.Unfoldable (replicateA)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error, error)
import Node.Encoding (Encoding(..))

class Decoder a where
  decode :: Read a

class Encoder a where
  encode :: a -> ByteString

newtype Read a = Read (ByteString -> ReadResult a)
derive instance newtypeRead :: Newtype (Read a) _
derive instance functorRead :: Functor Read

runReadEff
  :: ∀ m a
   . MonadEffect m
  => MonadThrow Error m
  => Decoder a
  => ByteString
  -> m (Tuple a ByteString)
runReadEff bs =
  case (unwrap decode) bs of
    Success res rem -> pure $ Tuple res rem 
    Failure err -> throwError (error err)

runReadEff'
  :: ∀ m a
   . MonadEffect m
  => MonadThrow Error m
  => Decoder a
  => ByteString -> m a
runReadEff' = map fst <<< runReadEff

instance applyRead :: Apply Read where
  apply fn fa = do
    fn' <- fn
    fa' <- fa
    pure $ fn' fa'

instance applicativeRead :: Applicative Read where
  pure val = Read (Success val)

instance bindRead :: Bind Read where
  bind (Read fa) f = Read \bs ->
    case fa bs of
      Success val rem -> unwrap (f val) rem
      Failure err -> Failure err


class GenericDecoder a where
  genericDecode' :: Read a

instance genericDecoderNoArguments :: GenericDecoder NoArguments where
  genericDecode' = Read $ Success NoArguments

instance genericDecoderProduct :: (GenericDecoder a, GenericDecoder b) => GenericDecoder (Product a b) where
  genericDecode' = Product <$> genericDecode' <*> genericDecode'

instance genericDecoderArgument :: Decoder a => GenericDecoder (Argument a) where
  genericDecode' = Argument <$> decode

instance genericDecoderConstructor :: GenericDecoder a => GenericDecoder (Constructor name a) where
  genericDecode' = Constructor <$> genericDecode'

genericDecode :: ∀ a rep. Generic a rep => GenericDecoder rep => Read a
genericDecode = to <$> genericDecode'

instance decoderLong :: Decoder (LongInt.Long Signed) where
  decode = flip Long.fromLowHighBits <$> Read BS.getInt32BE <*> Read BS.getInt32BE

instance decoderInt :: Decoder Int where
  decode = Read BS.getInt32BE

instance decoderShort :: Decoder Short where
  decode = wrap <$> Read BS.getInt16BE

instance decoderByte :: Decoder Byte where
  decode = wrap <$> Read BS.getInt8

instance decoderBool :: Decoder Boolean where
  decode = (_ /= 0) <$> Read BS.getInt8

instance decoderByteString :: Decoder ByteString where
  decode = Read <<< BS.take' =<< Read BS.getInt32BE

instance decoderString :: Decoder String where
  decode = flip BS.toString UTF8 <$> decode

instance decoderArray :: Decoder a => Decoder (Array a) where
  decode = do
    length <- Read BS.getInt32BE
    replicateA length decode

instance decoderMaybe :: Decoder a => Decoder (Maybe a) where
  decode = do
    exists <- decode
    if exists then Just <$> decode else pure Nothing


class GenericEncoder a where
  genericEncode' :: a -> ByteString

instance genericEncoderNoArguments :: GenericEncoder NoArguments where
  genericEncode' = mempty

instance genericEncoderProduct :: (GenericEncoder a, GenericEncoder b) => GenericEncoder (Product a b) where
  genericEncode' (Product a b) = genericEncode' a <> genericEncode' b

instance genericEncoderArgument :: Encoder a => GenericEncoder (Argument a) where
  genericEncode' (Argument a) = encode a

instance genericEncoderSum :: (GenericEncoder a, GenericEncoder b) => GenericEncoder (Sum a b) where
  genericEncode' (Inl a) = genericEncode' a
  genericEncode' (Inr a) = genericEncode' a

instance genericEncoderConstructor :: GenericEncoder a => GenericEncoder (Constructor name a) where
  genericEncode' (Constructor a) = genericEncode' a

genericEncode :: ∀ a rep. Generic a rep => GenericEncoder rep => a -> ByteString
genericEncode = genericEncode' <<< from

instance encoderLong :: Encoder (LongInt.Long Signed) where
  encode long = BS.fromInt32BE (Long.highBits long) <> BS.fromInt32BE (Long.lowBits long)

instance encoderInt :: Encoder Int where
  encode = BS.fromInt32BE

instance encoderShort :: Encoder Short where
  encode = BS.fromInt16BE <<< unwrap

instance encoderByte :: Encoder Byte where
  encode = BS.fromInt8 <<< unwrap

instance encoderBool :: Encoder Boolean where
  encode bool = BS.fromInt8 $ if bool then 1 else 0

instance encoderByteString :: Encoder ByteString where
  encode bs = BS.fromInt32BE (BS.length bs) <> bs

instance encoderString :: Encoder String where
  encode str = encode $ BS.fromString str UTF8 

instance encoderArray :: Encoder a => Encoder (Array a) where
  encode arr = BS.fromInt32BE (Array.length arr) <> foldMap encode arr

instance encoderMaybe :: Encoder a => Encoder (Maybe a) where
  encode Nothing = BS.fromInt8 0
  encode (Just a) = BS.fromInt8 1 <> encode a
