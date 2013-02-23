{-# LANGUAGE DefaultSignatures, EmptyDataDecls, FlexibleInstances,
    FunctionalDependencies, KindSignatures, OverlappingInstances,
    ScopedTypeVariables, TypeOperators, UndecidableInstances, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module:      Data.Aeson.Fixed.Types.Generic
-- Copyright:   (c) 2012 Jeffrey Shaw
--              (c) 2012 Bryan O'Sullivan
--              (c) 2011 Bas Van Dijk
--              (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Jeff Shaw <shawjef3@gmail.com
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Fixed.Types.Generic ( ) where

import Control.Applicative ((<*>), (<$>), (<|>), pure)
import Control.Monad.ST (ST)
import Data.Aeson.Fixed.Types.Class
import Data.Aeson.Fixed.Types.Internal
import Data.Bits (shiftR)
import Data.DList (DList, toList)
import Data.Monoid (mappend)
import Data.Text (pack, unpack)
import GHC.Generics
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

--------------------------------------------------------------------------------
-- Generic toJSON

instance (GToJSON r a) => GToJSON r (M1 i c a) where
    gToJSON = gToJSON . unM1
    {-# INLINE gToJSON #-}

instance (ToJSON r a) => GToJSON r (K1 i a) where
    gToJSON = toJSON . unK1
    {-# INLINE gToJSON #-}

instance GToJSON r U1 where
    gToJSON _ = emptyArray
    {-# INLINE gToJSON #-}

instance (ConsToJSON r a) => GToJSON r (C1 c a) where
    gToJSON = consToJSON . unM1
    {-# INLINE gToJSON #-}

instance ( GProductToValues r a, GProductToValues r b
         , ProductSize        a, ProductSize        b) => GToJSON r (a :*: b) where
    gToJSON p = Array $ V.create $ do
                  mv <- VM.unsafeNew lenProduct
                  gProductToValues mv 0 lenProduct p
                  return mv
        where
          lenProduct = unTagged2 (productSize :: Tagged2 (a :*: b) Int)
    {-# INLINE gToJSON #-}

instance (GObject r a, GObject r b) => GToJSON r (a :+: b) where
    gToJSON (L1 x) = Object $ gObject x
    gToJSON (R1 x) = Object $ gObject x
    {-# INLINE gToJSON #-}

--------------------------------------------------------------------------------

class ConsToJSON  r   f where consToJSON  ::           f a -> Value r
class ConsToJSON' r b f where consToJSON' :: Tagged b (f a -> Value r)

newtype Tagged s b = Tagged {unTagged :: b}

instance (IsRecord f b, ConsToJSON' r b f) => ConsToJSON r f where
    consToJSON = unTagged (consToJSON' :: Tagged b (f a -> Value r))
    {-# INLINE consToJSON #-}

instance (GRecordToPairs r f) => ConsToJSON' r True f where
    consToJSON' = Tagged (object . toList . gRecordToPairs)
    {-# INLINE consToJSON' #-}

instance GToJSON r f => ConsToJSON' r False f where
    consToJSON' = Tagged gToJSON
    {-# INLINE consToJSON' #-}

--------------------------------------------------------------------------------

class GRecordToPairs r f where
    gRecordToPairs :: f a -> DList (Pair r)

instance (GRecordToPairs r a, GRecordToPairs r b) => GRecordToPairs r (a :*: b) where
    gRecordToPairs (a :*: b) = gRecordToPairs a `mappend` gRecordToPairs b
    {-# INLINE gRecordToPairs #-}

instance (Selector s, GToJSON r a) => GRecordToPairs r (S1 s a) where
    gRecordToPairs m1 = pure (pack (selName m1), gToJSON (unM1 m1))
    {-# INLINE gRecordToPairs #-}

--------------------------------------------------------------------------------

class GProductToValues r f where
    gProductToValues :: VM.MVector s (Value r) -> Int -> Int -> f a -> ST s ()

instance (GProductToValues r a, GProductToValues r b) => GProductToValues r (a :*: b) where
    gProductToValues mv ix len (a :*: b) = do gProductToValues mv ix  lenL a
                                              gProductToValues mv ixR lenR b
        where
          lenL = len `shiftR` 1
          ixR  = ix + lenL
          lenR = len - lenL
    {-# INLINE gProductToValues #-}

instance (GToJSON r a) => GProductToValues r a where
    gProductToValues mv ix _ = VM.unsafeWrite mv ix . gToJSON
    {-# INLINE gProductToValues #-}

--------------------------------------------------------------------------------

class GObject r f where
    gObject :: f a -> Object r

instance (GObject r a, GObject r b) => GObject r (a :+: b) where
    gObject (L1 x) = gObject x
    gObject (R1 x) = gObject x
    {-# INLINE gObject #-}

instance (Constructor c, GToJSON r a, ConsToJSON r a) => GObject r (C1 c a) where
    gObject = H.singleton (pack $ conName (undefined :: t c a p)) . gToJSON
    {-# INLINE gObject #-}

--------------------------------------------------------------------------------
-- Generic parseJSON

instance (GFromJSON r a) => GFromJSON r (M1 i c a) where
    gParseJSON = fmap M1 . gParseJSON
    {-# INLINE gParseJSON #-}

instance (FromJSON r a) => GFromJSON r (K1 i a) where
    gParseJSON = fmap K1 . parseJSON
    {-# INLINE gParseJSON #-}

instance GFromJSON r U1 where
    gParseJSON v
        | isEmptyArray v = pure U1
        | otherwise      = typeMismatch "unit constructor (U1)" v
    {-# INLINE gParseJSON #-}

instance (ConsFromJSON r a) => GFromJSON r (C1 c a) where
    gParseJSON = fmap M1 . consParseJSON
    {-# INLINE gParseJSON #-}

instance ( GFromProduct r a, GFromProduct r b
         , ProductSize a, ProductSize b) => GFromJSON r (a :*: b) where
    gParseJSON (Array arr)
        | lenArray == lenProduct = gParseProduct arr 0 lenProduct
        | otherwise =
            fail $ "When expecting a product of " ++ show lenProduct ++
                   " values, encountered an Array of " ++ show lenArray ++
                   " elements instead"
        where
          lenArray = V.length arr
          lenProduct = unTagged2 (productSize :: Tagged2 (a :*: b) Int)

    gParseJSON v = typeMismatch "product (:*:)" v
    {-# INLINE gParseJSON #-}

instance (GFromSum r a, GFromSum r b) => GFromJSON r (a :+: b) where
    gParseJSON (Object (H.toList -> [keyVal@(key, _)])) =
        case gParseSum keyVal of
          Nothing -> notFound $ unpack key
          Just p  -> p
    gParseJSON v = typeMismatch "sum (:+:)" v
    {-# INLINE gParseJSON #-}

notFound :: String -> Parser a
notFound key = fail $ "The key \"" ++ key ++ "\" was not found"
{-# INLINE notFound #-}

--------------------------------------------------------------------------------

class ConsFromJSON  r   f where consParseJSON  ::           Value r -> Parser (f a)
class ConsFromJSON' r b f where consParseJSON' :: Tagged b (Value r -> Parser (f a))

instance (IsRecord f b, ConsFromJSON' r b f) => ConsFromJSON r f where
    consParseJSON = unTagged (consParseJSON' :: Tagged b (Value r -> Parser (f a)))
    {-# INLINE consParseJSON #-}

instance (GFromRecord r f) => ConsFromJSON' r True f where
    consParseJSON' = Tagged parseRecord
        where
          parseRecord (Object obj) = gParseRecord obj
          parseRecord v = typeMismatch "record (:*:)" v
    {-# INLINE consParseJSON' #-}

instance (GFromJSON r f) => ConsFromJSON' r False f where
    consParseJSON' = Tagged gParseJSON
    {-# INLINE consParseJSON' #-}

--------------------------------------------------------------------------------

class GFromRecord r f where
    gParseRecord :: Object r -> Parser (f a)

instance (GFromRecord r a, GFromRecord r b) => GFromRecord r (a :*: b) where
    gParseRecord obj = (:*:) <$> gParseRecord obj <*> gParseRecord obj
    {-# INLINE gParseRecord #-}

instance (Selector s, GFromJSON r a) => GFromRecord r (S1 s a) where
    gParseRecord = maybe (notFound key) gParseJSON . H.lookup (T.pack key)
        where
          key = selName (undefined :: t s a p)
    {-# INLINE gParseRecord #-}

--------------------------------------------------------------------------------

class ProductSize f where
    productSize :: Tagged2 f Int

newtype Tagged2 (s :: * -> *) b = Tagged2 {unTagged2 :: b}

instance (ProductSize a, ProductSize b) => ProductSize (a :*: b) where
    productSize = Tagged2 $ unTagged2 (productSize :: Tagged2 a Int) +
                            unTagged2 (productSize :: Tagged2 b Int)

instance ProductSize (S1 s a) where
    productSize = Tagged2 1

--------------------------------------------------------------------------------

class GFromProduct r f where
    gParseProduct :: Array r -> Int -> Int -> Parser (f a)

instance (GFromProduct r a, GFromProduct r b) => GFromProduct r (a :*: b) where
    gParseProduct arr ix len = (:*:) <$> gParseProduct arr ix  lenL
                                     <*> gParseProduct arr ixR lenR
        where
          lenL = len `shiftR` 1
          ixR  = ix + lenL
          lenR = len - lenL
    {-# INLINE gParseProduct #-}

instance (GFromJSON r a) => GFromProduct r (S1 s a) where
    gParseProduct arr ix _ = gParseJSON $ V.unsafeIndex arr ix
    {-# INLINE gParseProduct #-}

--------------------------------------------------------------------------------

class GFromSum r f where
    gParseSum :: Pair r -> Maybe (Parser (f a))

instance (GFromSum r a, GFromSum r b) => GFromSum r (a :+: b) where
    gParseSum keyVal = (fmap L1 <$> gParseSum keyVal) <|>
                       (fmap R1 <$> gParseSum keyVal)
    {-# INLINE gParseSum #-}

instance (Constructor c, GFromJSON r a, ConsFromJSON r a) => GFromSum r (C1 c a) where
    gParseSum (key, value)
        | key == pack (conName (undefined :: t c a p)) = Just $ gParseJSON value
        | otherwise = Nothing
    {-# INLINE gParseSum #-}

--------------------------------------------------------------------------------

class IsRecord (f :: * -> *) b | f -> b

data True
data False

instance (IsRecord f b) => IsRecord (f :*: g) b
instance IsRecord (M1 S NoSelector f) False
instance (IsRecord f b) => IsRecord (M1 S c f) b
instance IsRecord (K1 i c) True
instance IsRecord U1 False

--------------------------------------------------------------------------------
