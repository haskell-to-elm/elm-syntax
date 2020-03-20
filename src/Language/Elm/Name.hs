{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
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
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString)
  deriving anyclass (Hashable)

data Qualified = Qualified Module Text
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

newtype Field = Field Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString)
  deriving anyclass (Hashable)

newtype Constructor = Constructor Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString)
  deriving anyclass (Hashable)

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
