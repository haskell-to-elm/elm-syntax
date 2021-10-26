{-# language DeriveAnyClass #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TemplateHaskell #-}
{-# language TupleSections #-}
module Language.Elm.Expression where

import Bound
import Bound.Var (unvar)
import Control.Monad
import Data.Bifoldable
import Data.Bifunctor
import Data.Eq.Deriving
import Data.Ord.Deriving
import Data.String
import Data.Text (Text)
import Text.Show.Deriving

import qualified Language.Elm.Name as Name
import Language.Elm.Pattern (Pattern)
import qualified Language.Elm.Pattern as Pattern

data Expression v
  = Var v
  | Global Name.Qualified
  | App (Expression v) (Expression v)
  | Let (Expression v) (Scope () Expression v)
  | Lam (Scope () Expression v)
  | Record [(Name.Field, Expression v)]
  | Proj Name.Field
  | Case (Expression v) [(Pattern Int, Scope Int Expression v)]
  | List [Expression v]
  | String !Text
  | Int !Integer
  | Float !Double
  deriving (Functor, Foldable, Traversable)

instance Applicative Expression where
  pure = Var
  (<*>) = ap

instance Monad Expression where
  expression >>= f =
    case expression of
      Var v ->
        f v

      Global g ->
        Global g
  
      App g x ->
        App (g >>= f) (x >>= f)

      Let e s ->
        Let (e >>= f) (s >>>= f)

      Lam e -> 
        Lam (e >>>= f)

      Record fields ->
        Record (map (fmap (>>= f)) fields)
  
      Proj fieldName ->
        Proj fieldName

      Case e patterns ->
        Case (e >>= f) (map (fmap (>>>= f)) patterns)
  
      List es -> 
        List (map (>>= f) es)

      String text ->
        String text

      Int integer -> 
        Int integer

      Float double -> 
        Float double


bind :: forall v v'. (Name.Qualified -> Expression v') -> (v -> Expression v') -> Expression v -> Expression v'
bind global var expression =
  case expression of
    Var v ->
      var v

    Global g ->
      global g

    App t1 t2 ->
      App (bind global var t1) (bind global var t2)

    Let e s ->
      Let (bind global var e) (bindScope s)

    Lam s ->
      Lam (bindScope s)

    Record fields ->
      Record $ second (bind global var) <$> fields

    Proj fname ->
      Proj fname

    Case scrutinee branches ->
      Case
        (bind global var scrutinee)
        (second bindScope <$> branches)

    List es ->
      List $ bind global var <$> es

    String s ->
      String s

    Int i ->
      Int i

    Float f ->
      Float f
  where
    bindScope :: Scope b Expression v -> Scope b Expression v'
    bindScope =
      toScope .
      bind (fmap F . global) (unvar (pure . B) (fmap F . var)) .
      fromScope

deriveEq1 ''Expression
deriveOrd1 ''Expression
deriveShow1 ''Expression

deriving instance Eq v => Eq (Expression v)
deriving instance Ord v => Ord (Expression v)
deriving instance Show v => Show (Expression v)

instance IsString (Expression v) where
  fromString = Global . fromString

apps :: Foldable f => Expression v -> f (Expression v) -> Expression v
apps = foldl App

appsView :: Expression v -> (Expression v, [Expression v])
appsView = go mempty
  where
    go args expr =
      case expr of
        App e1 e2 ->
          go (e2 : args) e1

        _ ->
          (expr, args)

if_ :: Expression v -> Expression v -> Expression v -> Expression v
if_ bool_ true false =
  Case bool_
    [ (Pattern.Con "Basics.True" [], Scope $ pure $ pure true)
    , (Pattern.Con "Basics.False" [], Scope $ pure $ pure false)
    ]

(|>) :: Expression v -> Expression v -> Expression v
(|>) e1 e2 = apps "Basics.|>" [e1, e2]

(<|) :: Expression v -> Expression v -> Expression v
(<|) e1 e2 = apps "Basics.<|" [e1, e2]

(<<) :: Expression v -> Expression v -> Expression v
(<<) e1 e2 = apps "Basics.<<" [e1, e2]

(>>) :: Expression v -> Expression v -> Expression v
(>>) e1 e2 = apps "Basics.>>" [e1, e2]

(++) :: Expression v -> Expression v -> Expression v
(++) e1 e2 = apps "Basics.++" [e1, e2]

tuple :: Expression v -> Expression v -> Expression v
tuple e1 e2 = apps "Basics.," [e1, e2]

lets :: Eq b => [(b, Expression v)] -> Scope b Expression v -> Expression v
lets =
  go (error "Language.Elm.Expression.lets unbound var") id
  where
    go :: Eq b => (b -> v') -> (v -> v') -> [(b, Expression v)] -> Scope b Expression v -> Expression v'
    go boundVar freeVar bindings scope =
      case bindings of
        [] ->
          unvar boundVar freeVar <$> fromScope scope

        (v, e):bindings' ->
          Let (freeVar <$> e) $
            toScope $
            go
              (\b -> if b == v then B () else F $ boundVar b)
              (F . freeVar)
              bindings'
              scope

foldMapGlobals
  :: Monoid m
  => (Name.Qualified -> m)
  -> Expression v
  -> m
foldMapGlobals f expr =
  case expr of
    Var _ ->
      mempty

    Global qname ->
      f qname

    App e1 e2 ->
      foldMapGlobals f e1 <> foldMapGlobals f e2

    Let e s ->
      foldMapGlobals f e <> foldMapGlobals f (Bound.fromScope s)

    Lam s ->
      foldMapGlobals f (Bound.fromScope s)

    Record fields ->
      foldMap (foldMap (foldMapGlobals f)) fields

    Proj _ ->
      mempty

    Case e branches ->
      foldMapGlobals f e <>
      foldMap
        (bifoldMap (Pattern.foldMapGlobals f) (foldMapGlobals f .Bound.fromScope))
        branches

    List es ->
      foldMap (foldMapGlobals f) es

    String _ ->
      mempty

    Int _ ->
      mempty

    Float _ ->
      mempty
