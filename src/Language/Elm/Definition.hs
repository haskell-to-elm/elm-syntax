module Language.Elm.Definition where

import Data.Void

import Bound (Scope)
import qualified Bound

import Language.Elm.Expression (Expression)
import qualified Language.Elm.Expression as Expression
import qualified Language.Elm.Name as Name
import Language.Elm.Type (Type)
import qualified Language.Elm.Type as Type

data Definition
  = Constant !Name.Qualified !Int (Scope Int Type Void) (Expression Void)
  | Type !Name.Qualified !Int [(Name.Constructor, [Scope Int Type Void])]
  | Alias !Name.Qualified !Int (Scope Int Type Void)
  deriving (Eq, Ord, Show)

name :: Definition -> Name.Qualified
name (Constant n _ _ _) = n
name (Type n _ _) = n
name (Alias n _ _) = n

foldMapGlobals
  :: Monoid m
  => (Name.Qualified -> m)
  -> Definition
  -> m
foldMapGlobals f def =
  case def of
    Constant qname _ type_ expr ->
      f qname <>
      Type.foldMapGlobals f (Bound.fromScope type_) <>
      Expression.foldMapGlobals f expr

    Type qname _ constrs ->
      f qname <>
      foldMap (foldMap (foldMap (Type.foldMapGlobals f . Bound.fromScope))) constrs

    Alias qname _ type_ ->
      f qname <>
      Type.foldMapGlobals f (Bound.fromScope type_)
