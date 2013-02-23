{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- |
-- Module:      Data.Aeson.Encode
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently serialize a JSON value.
--
-- Most frequently, you'll probably want to encode straight to UTF-8
-- (the standard JSON encoding) using 'encode'.
--
-- You can convert a 'Builder' (as returned by 'fromValue') to a
-- string using e.g. 'toLazyText'.

module Data.Aeson.Encode
    (
      fromValue
    , encode
    ) where

import Data.Fixed
import Data.Aeson.Types (ToJSON(..), Value(..))
import Data.Monoid (mappend, mconcat)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Numeric (showHex)
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Encode a JSON value to a 'Builder'.  You can convert this to a
-- string using e.g. 'toLazyText', or encode straight to UTF-8 (the
-- standard JSON encoding) using 'encode'.
fromValue :: (HasResolution r) => Value r -> Builder
fromValue Null = {-# SCC "fromValue/Null" #-} "null"
fromValue (Bool b) = {-# SCC "fromValue/Bool" #-}
                     if b then "true" else "false"
fromValue (Number n) = {-# SCC "fromValue/Number" #-} fromNumber n
fromValue (String s) = {-# SCC "fromValue/String" #-} string s
fromValue (Array v)
    | V.null v = {-# SCC "fromValue/Array" #-} "[]"
    | otherwise = {-# SCC "fromValue/Array" #-}
                  singleton '[' <>
                  fromValue (V.unsafeHead v) <>
                  V.foldr f (singleton ']') (V.unsafeTail v)
  where f a z = singleton ',' <> fromValue a <> z
fromValue (Object m) = {-# SCC "fromValue/Object" #-}
    case H.toList m of
      (x:xs) -> singleton '{' <> one x <> foldr f (singleton '}') xs
      _      -> "{}"
  where f a z     = singleton ',' <> one a <> z
        one (k,v) = string k <> singleton ':' <> fromValue v

string :: T.Text -> Builder
string s = {-# SCC "string" #-} singleton '"' <> quote s <> singleton '"'
  where
    quote q = case T.uncons t of
                Nothing     -> fromText h
                Just (!c,t') -> fromText h <> escape c <> quote t'
        where (h,t) = {-# SCC "break" #-} T.break isEscape q
    isEscape c = c == '\"' || c == '\\' || c < '\x20'
    escape '\"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c
        | c < '\x20' = fromString $ "\\u" ++ replicate (4 - length h) '0' ++ h
        | otherwise  = singleton c
        where h = showHex (fromEnum c) ""

fromNumber :: (HasResolution r) => Fixed r -> Builder
fromNumber f =
    let wholes = truncate f
        decimals = truncate $ (f `mod'` 1) * (fromRational $ toRational $ resolution f)
    in mconcat [decimal wholes, ".", decimal decimals]

-- | Efficiently serialize a JSON value as a lazy 'L.ByteString'.
encode :: (HasResolution r, ToJSON r a) => a -> L.ByteString
encode = encodeUtf8 . toLazyText . fromValue . toJSON

(<>) :: Builder -> Builder -> Builder
(<>) = mappend
{-# INLINE (<>) #-}
infixr 6 <>
