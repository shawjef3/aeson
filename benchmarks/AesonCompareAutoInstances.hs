{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell, MultiParamTypeClasses, FlexibleContexts #-}

module Main where

--------------------------------------------------------------------------------

import Criterion.Main

import Control.DeepSeq (NFData, rnf, deepseq)

import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.Fixed
import GHC.Generics (Generic)

import Data.Aeson.Types
import Data.Aeson.TH (mkToJSON, mkParseJSON)
import qualified Data.Aeson.Generic as G (fromJSON, toJSON)

--------------------------------------------------------------------------------

-- Taken from the documentation of Data.Aeson.TH:
data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Double
                  , testTwo   :: Bool
                  , testThree :: D a
                  } deriving (Show, Eq, Generic, Data, Typeable)

instance NFData a => NFData (D a) where
    rnf Nullary         = ()
    rnf (Unary n)       = rnf n
    rnf (Product s c x) = s `deepseq` c `deepseq` rnf x
    rnf (Record d b y)  = d `deepseq` b `deepseq` rnf y

type T = D (D (D ()))

d :: T
d = Record
    { testOne = 1234.56789
    , testTwo = True
    , testThree = Product "Hello World!" 'a' $
                    Record
                    { testOne   = 9876.54321
                    , testTwo   = False
                    , testThree = Product "Yeehaa!!!" '\n' Nullary
                    }
    }

instance ToJSON  E9 a => ToJSON E9  (D a)
instance FromJSON E9 a => FromJSON E9 (D a)

thDToJSON :: ToJSON E9 a => D a -> Value E9
thDToJSON = $(mkToJSON id ''D)

thDParseJSON :: FromJSON E9 a => Value E9 -> Parser (D a)
thDParseJSON = $(mkParseJSON id ''D)

thDFromJSON :: FromJSON E9 a => Value E9 -> Result (D a)
thDFromJSON = parse thDParseJSON

--------------------------------------------------------------------------------

data BigRecord = BigRecord
    { field01 :: !Nano, field02 :: !Nano, field03 :: !Nano, field04 :: !Nano, field05 :: !Nano
    , field06 :: !Nano, field07 :: !Nano, field08 :: !Nano, field09 :: !Nano, field10 :: !Nano
    , field11 :: !Nano, field12 :: !Nano, field13 :: !Nano, field14 :: !Nano, field15 :: !Nano
    , field16 :: !Nano, field17 :: !Nano, field18 :: !Nano, field19 :: !Nano, field20 :: !Nano
    , field21 :: !Nano, field22 :: !Nano, field23 :: !Nano, field24 :: !Nano, field25 :: !Nano
    } deriving (Show, Eq, Generic, Data, Typeable)

instance NFData BigRecord

bigRecord = BigRecord 1   2  3  4  5
                      6   7  8  9 10
                      11 12 13 14 15
                      16 17 18 19 20
                      21 22 23 24 25

instance ToJSON E9 BigRecord
instance FromJSON E9 BigRecord

thBigRecordToJSON :: BigRecord -> Value E9
thBigRecordToJSON = $(mkToJSON id ''BigRecord)

thBigRecordParseJSON :: Value E9 -> Parser BigRecord
thBigRecordParseJSON = $(mkParseJSON id ''BigRecord)

thBigRecordFromJSON :: Value E9 -> Result BigRecord
thBigRecordFromJSON = parse thBigRecordParseJSON

--------------------------------------------------------------------------------

data BigProduct = BigProduct
    !Nano !Nano !Nano !Nano !Nano
    !Nano !Nano !Nano !Nano !Nano
    !Nano !Nano !Nano !Nano !Nano
    !Nano !Nano !Nano !Nano !Nano
    !Nano !Nano !Nano !Nano !Nano
    deriving (Show, Eq, Generic, Data, Typeable)

instance NFData BigProduct

bigProduct = BigProduct 1   2  3  4  5
                        6   7  8  9 10
                        11 12 13 14 15
                        16 17 18 19 20
                        21 22 23 24 25

instance ToJSON E9  BigProduct
instance FromJSON E9 BigProduct

thBigProductToJSON :: BigProduct -> Value E9
thBigProductToJSON = $(mkToJSON id ''BigProduct)

thBigProductParseJSON :: Value E9 -> Parser BigProduct
thBigProductParseJSON = $(mkParseJSON id ''BigProduct)

thBigProductFromJSON :: Value E9 -> Result BigProduct
thBigProductFromJSON = parse thBigProductParseJSON

--------------------------------------------------------------------------------

data BigSum = F01 | F02 | F03 | F04 | F05
            | F06 | F07 | F08 | F09 | F10
            | F11 | F12 | F13 | F14 | F15
            | F16 | F17 | F18 | F19 | F20
            | F21 | F22 | F23 | F24 | F25
    deriving (Show, Eq, Generic, Data, Typeable)

instance NFData BigSum

bigSum = F25

instance ToJSON E9 BigSum
instance FromJSON E9 BigSum

thBigSumToJSON :: BigSum -> Value E9
thBigSumToJSON = $(mkToJSON id ''BigSum)

thBigSumParseJSON :: Value E9 -> Parser BigSum
thBigSumParseJSON = $(mkParseJSON id ''BigSum)

thBigSumFromJSON :: Value E9 -> Result BigSum
thBigSumFromJSON = parse thBigSumParseJSON

--------------------------------------------------------------------------------

type FJ a = Value E9 -> Result a

main :: IO ()
main = defaultMain
  [ let v = thDToJSON d
    in d `deepseq` v `deepseq`
       bgroup "D"
       [ group "toJSON"   (nf thDToJSON d)
                          (nf (G.toJSON :: Data a => a -> Value E9)  d)
                          (nf toJSON    d)
       , group "fromJSON" (nf (thDFromJSON :: FJ T) v)
                          (nf (G.fromJSON  :: FJ T) v)
                          (nf (fromJSON    :: FJ T) v)
       ]
  , let v = thBigRecordToJSON bigRecord
    in bigRecord `deepseq` v `deepseq`
       bgroup "BigRecord"
       [ group "toJSON"   (nf thBigRecordToJSON bigRecord)
                          (nf (G.toJSON :: Data a => a -> Value E9)          bigRecord)
                          (nf toJSON            bigRecord)
       , group "fromJSON" (nf (thBigRecordFromJSON :: FJ BigRecord) v)
                          (nf (G.fromJSON          :: FJ BigRecord) v)
                          (nf (fromJSON            :: FJ BigRecord) v)
       ]
  , let v = thBigProductToJSON bigProduct
    in bigProduct `deepseq` v `deepseq`
       bgroup "BigProduct"
       [ group "toJSON"   (nf thBigProductToJSON bigProduct)
                          (nf (G.toJSON :: Data a => a -> Value E9)           bigProduct)
                          (nf toJSON             bigProduct)
       , group "fromJSON" (nf (thBigProductFromJSON :: FJ BigProduct) v)
                          (nf (G.fromJSON           :: FJ BigProduct) v)
                          (nf (fromJSON             :: FJ BigProduct) v)
       ]
  , let v = thBigSumToJSON bigSum
    in bigSum `deepseq` v `deepseq`
       bgroup "BigSum"
       [ group "toJSON"   (nf thBigSumToJSON bigSum)
                          (nf (G.toJSON :: Data a => a -> Value E9)       bigSum)
                          (nf toJSON         bigSum)
       , group "fromJSON" (nf (thBigSumFromJSON :: FJ BigSum) v)
                          (nf (G.fromJSON       :: FJ BigSum) v)
                          (nf (fromJSON         :: FJ BigSum) v)
       ]
  ]

group n th syb gen = bgroup n [ bench "th"      th
                              , bench "syb"     syb
                              , bench "generic" gen
                              ]
