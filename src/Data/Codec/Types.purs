module Data.Codec.Types where

import Data.Newtype (class Newtype, unwrap, wrap)

newtype Byte = Byte Int
derive instance newtypeByte :: Newtype Byte _

newtype Short = Short Int
derive instance newtypeShort :: Newtype Short _

class Integral a where
  toInt :: a -> Int
  fromInt :: Int -> a

instance byteIntegral :: Integral Byte where
  toInt = unwrap
  fromInt = wrap

instance shortIntegral :: Integral Short where
  toInt = unwrap
  fromInt = wrap
