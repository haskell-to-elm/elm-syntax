{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveTraversable #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
module Language.Elm.Type where

import Control.Monad
import Data.Bifunctor
import Data.Eq.Deriving (deriveEq1)
import Data.Foldable
import Data.Ord.Deriving (deriveOrd1)
import Data.String
import Text.Show.Deriving (deriveShow1)

import qualified Language.Elm.Name as Name

data Type v
  = Var v
  | Global Name.Qualified
  | App (Type v) (Type v)
  | Fun (Type v) (Type v)
  | Record [(Name.Field, Type v)]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

deriveEq1 ''Type
deriveOrd1 ''Type
deriveShow1 ''Type

instance Applicative Type where
  pure = Var
  (<*>) = ap

instance Monad Type where
  (>>=) =
    flip $ bind Global

bind :: (Name.Qualified -> Type v') -> (v -> Type v') -> Type v -> Type v'
bind global var type_ =
  case type_ of
    Var v ->
      var v

    Global g ->
      global g

    App t1 t2 ->
      App (bind global var t1) (bind global var t2)

    Fun t1 t2 ->
      Fun (bind global var t1) (bind global var t2)

    Record fields ->
      Record $ second (bind global var) <$> fields

instance IsString (Type v) where
  fromString = Global . fromString

apps :: Type v -> [Type v] -> Type v
apps = foldl' App

appsView :: Type v -> (Type v, [Type v])
appsView = go mempty
  where
    go args typ =
      case typ of
        App t1 t2 ->
          go (t2 : args) t1

        _ ->
          (typ, args)

funs :: [Type v] -> Type v -> Type v
funs args ret =
  foldr Fun ret args

tuple :: Type v -> Type v -> Type v
tuple t1 t2 = apps "Basics.," [t1, t2]

foldMapGlobals
  :: Monoid m
  => (Name.Qualified -> m)
  -> Type v
  -> m
foldMapGlobals f type_ =
  case type_ of
    Var _ ->
      mempty

    Global qname ->
      f qname

    App t1 t2 ->
      foldMapGlobals f t1 <> foldMapGlobals f t2

    Fun t1 t2 ->
      foldMapGlobals f t1 <> foldMapGlobals f t2

    Record fields ->
      foldMap (foldMap (foldMapGlobals f)) fields
