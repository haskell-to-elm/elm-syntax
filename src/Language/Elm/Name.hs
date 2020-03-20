{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Elm.Name where

import Data.Bifunctor
import qualified Data.Char as Char
import Data.Hashable
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

type Module = [Text]

newtype Local = Local Text
  deriving (Eq, Ord, Show, Generic)

data Qualified = Qualified Module Text
  deriving (Eq, Ord, Show, Generic)

newtype Field = Field Text
  deriving (Eq, Ord, Show, Generic)

newtype Constructor = Constructor Text
  deriving (Eq, Ord, Show, Generic)

isConstructor :: Qualified -> Bool
isConstructor name =
  case name of
    "List.::" ->
      True

    "Basics.," ->
      True

    "Basics.()" ->
      True

    Qualified _ (Text.uncons -> Just (firstChar, _)) ->
      Char.isUpper firstChar

    _ ->
      False

instance IsString Local where
  fromString = Local . fromString

instance Hashable Local where
  hashWithSalt s (Local t) =
    hashWithSalt s t

instance IsString Qualified where
  fromString s =
    case unsnoc $ Text.splitOn "." $ fromString s of
      Nothing ->
        error "Empty name"

      Just ([], x) ->
        error $ "Unqualified name " <> show x

      Just (xs, x) ->
        Qualified xs x
    where
      unsnoc :: [a] -> Maybe ([a], a)
      unsnoc [] = Nothing
      unsnoc (a:as) = Just $ go a as

      go :: a -> [a] -> ([a], a)
      go a [] = ([], a)
      go a (a':as) = first (a:) $ go a' as

instance Hashable Qualified

instance IsString Field where
  fromString = Field . fromString

instance Hashable Field where
  hashWithSalt s (Field t) =
    hashWithSalt s t

instance IsString Constructor where
  fromString = Constructor . fromString

instance Hashable Constructor where
  hashWithSalt s (Constructor t) =
    hashWithSalt s t
