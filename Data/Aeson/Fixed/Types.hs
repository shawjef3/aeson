{-# LANGUAGE CPP #-}

-- |
-- Module:      Data.Aeson.Types
-- Copyright:   (c) 2011 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Fixed.Types
    (
    -- * Core JSON types
      Value(..)
    , Array
    , emptyArray
    , Pair
    , Object
    , emptyObject
    -- * Convenience types and functions
    , DotNetTime(..)
    , typeMismatch
    -- * Type conversion
    , Parser
    , Result(..)
    , FromJSON(..)
    , fromJSON
    , parse
    , parseEither
    , parseMaybe
    , ToJSON(..)
    -- * Constructors and accessors
    , (.=)
    , (.:)
    , (.:?)
    , (.!=)
    , object
    ) where

import Data.Aeson.Fixed.Types.Class
import Data.Aeson.Fixed.Types.Internal

#ifdef GENERICS
import Data.Aeson.Fixed.Types.Generic ()
#endif
