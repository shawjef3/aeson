{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, RecordWildCards,
    ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Monad
import Control.Applicative
import Data.Aeson.Encode
import Data.Aeson.Parser (value)
import Data.Aeson.Types
import Data.Data (Typeable, Data)
import Data.Decimal
import Data.Text (Text)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))
import qualified Data.Aeson.Generic as G
import qualified Data.Attoparsec.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import qualified Data.Map as Map

encodeDecimal :: Decimal -> Bool
encodeDecimal d = encode d == L.pack (show d)

roundTrip :: (FromJSON a, ToJSON a) => (a -> a -> Bool) -> a -> a -> Bool
roundTrip eq _ i =
    case fmap fromJSON . L.parse value . encode . toJSON $ i of
      L.Done _ (Success v) -> v `eq` i
      _                    -> False

roundTripEq :: (Eq a, FromJSON a, ToJSON a) => a -> a -> Bool
roundTripEq x y = roundTrip (==) x y

genericTo :: (Data a, ToJSON a) => a -> a -> Bool
genericTo _ v = G.toJSON v == toJSON v

genericFrom :: (Eq a, Data a, ToJSON a) => a -> a -> Bool
genericFrom _ v = G.fromJSON (toJSON v) == Success v

toFromJSON :: (Arbitrary a, Eq a, FromJSON a, ToJSON a) => a -> Bool
toFromJSON x = case fromJSON . toJSON $ x of
                Error _ -> False
                Success x' -> x == x'

genericToFromJSON :: (Arbitrary a, Eq a, Data a) => a -> Bool
genericToFromJSON x = case G.fromJSON . G.toJSON $ x of
                Error _ -> False
                Success x' -> x == x'

data Foo = Foo {
      fooDecimal :: Decimal
    , fooTuple :: (String, Text, Int)
    -- This definition causes an infinite loop in genericTo and genericFrom!
    -- , fooMap :: Map.Map String Foo
    , fooMap :: Map.Map String (Text,Int)
    } deriving (Show, Typeable, Data)

instance Eq Foo where
    a == b = fooDecimal a == fooDecimal b &&
             fooTuple a == fooTuple b

instance ToJSON Foo where
    toJSON Foo{..} = object [ "fooDecimal" .= fooDecimal
                            , "fooTuple" .= fooTuple
                            , "fooMap" .= fooMap
                            ]

instance FromJSON Foo where
    parseJSON (Object v) = Foo <$>
                           v .: "fooDecimal" <*>
                           v .: "fooTuple" <*>
                           v .: "fooMap"
    parseJSON _ = empty

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map.Map k v) where
    arbitrary = Map.fromList <$> arbitrary

instance Arbitrary Foo where
    arbitrary = liftM3 Foo arbitrary arbitrary arbitrary

data UFoo = UFoo {
      _UFooInt :: Int
    , uFooInt :: Int
    } deriving (Show, Eq, Data, Typeable)

instance Arbitrary UFoo where
    arbitrary = UFoo <$> arbitrary <*> arbitrary
        where _ = uFooInt

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
  testGroup "encode" [
      testProperty "encodeDecimal" encodeDecimal
    ],
  testGroup "genericFrom" [
      testProperty "Bool" $ genericFrom True
    , testProperty "Decimal" $ genericFrom (1::Decimal)
    , testProperty "Foo" $ genericFrom (undefined::Foo)
    ],
  testGroup "genericTo" [
      testProperty "Bool" $ genericTo True
    , testProperty "Decimal" $ genericTo (1::Decimal)
    , testProperty "Foo" $ genericTo (undefined::Foo)
    ],
  testGroup "roundTrip" [
      testProperty "Bool" $ roundTripEq True
    , testProperty "Int" $ roundTripEq (1::Int)
    , testProperty "Integer" $ roundTripEq (1::Integer)
    , testProperty "Decimal" $ roundTripEq (1::Decimal)
    , testProperty "String" $ roundTripEq (""::String)
    , testProperty "Text" $ roundTripEq T.empty
    , testProperty "Foo" $ roundTripEq (undefined::Foo)
    ],
  testGroup "toFromJSON" [
      testProperty "Integer" (toFromJSON :: Integer -> Bool)
    , testProperty "Decimal" (toFromJSON :: Decimal -> Bool)
    , testProperty "Maybe Integer" (toFromJSON :: Maybe Integer -> Bool)
    , testProperty "Either Integer Integer" (toFromJSON :: Either Integer Integer -> Bool)
    ],
  testGroup "genericToFromJSON" [
      testProperty "_UFoo" (genericToFromJSON :: UFoo -> Bool)
    ]
  ]
