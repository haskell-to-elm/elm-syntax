{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
module Language.Elm.Simplification
  ( simplifyDefinition
  , simplifyExpression
  ) where

import Protolude

import Bound
import qualified Bound.Scope as Scope
import Bound.Var (unvar)
import Data.Foldable (fold)
import Data.Text (Text)
import qualified Data.Text as Text

import Language.Elm.Definition (Definition)
import qualified Language.Elm.Definition as Definition
import Language.Elm.Expression (Expression)
import qualified Language.Elm.Expression as Expression
import qualified Language.Elm.Name as Name
import Language.Elm.Pattern (Pattern)
import qualified Language.Elm.Pattern as Pattern

-- | Perform 'simplifyExpression' on all 'Expression's inside the given
-- 'Definition'.
simplifyDefinition
  :: Definition
  -> Definition
simplifyDefinition def =
  case def of
    Definition.Constant name type_ expr ->
      Definition.Constant name type_ $ simplifyExpression expr

    Definition.Type {} ->
      def

    Definition.Alias {} ->
      def

-- | Run the following simplifications on the given expression:
--
-- * @identity x = x@
-- * @(f >> g) x = g (f x)@
-- * @f >> identity = f@
-- * @identity >> f = f@
-- * @(f << g) x = f (g x)@
-- * @f << identity = f@
-- * @identity << f= f@
-- * @identity <| x = x@
-- * @x |> identity = x@
-- * @x :: [y, z, ...] = [x, y, z, ...]@
-- * Calls to @String.join@, @String.concat@, @List.concat@, and @++@ with
--   known arguments are simplified. For example,
--   @String.join "/" [Config.api, "endpoint"] = Config.api ++ "/endpoint"@
-- * @(\x. e x) = e@
-- * Inline @x@ in @e'@ in
--   @
--   let x = e in e'
--   @
--   if either:
--   - @e@ is freely duplicable, e.g. it's just a variable or a numeric literal.
--   - @x@ occurs zero or one times in @e'@.
-- * @
--   case e of
--     ... prefixBranches
--     pat -> branch
--     ...
--   @
--   is simplified to @let xs = es in branch@ provided that @e@ matches none of
--   @prefixBranches@ and that it matches @pat@.
-- * case-of-case
--
simplifyExpression
  :: Expression v
  -> Expression v
simplifyExpression expr =
  simplifyApplication expr []

simplifyApplication
  :: Expression v
  -> [Expression v]
  -> Expression v
simplifyApplication expr args =
  case (expr, args) of
    (Expression.Var _, _) ->
      Expression.apps expr args

    (Expression.Global "Basics.identity", arg:args') ->
      simplifyApplication arg args'

    (Expression.Global "Basics.>>", f:g:arg:args') ->
      simplifyApplication (Expression.App g (Expression.App f arg)) args'

    (Expression.Global "Basics.>>", [f, Expression.Global "Basics.identity"]) ->
      f

    (Expression.Global "Basics.>>", [Expression.Global "Basics.identity", f]) ->
      f

    (Expression.Global "Basics.<<", f:g:arg:args') ->
      simplifyApplication (Expression.App f (Expression.App g arg)) args'

    (Expression.Global "Basics.<<", [f, Expression.Global "Basics.identity"]) ->
      f

    (Expression.Global "Basics.<<", [Expression.Global "Basics.identity", f]) ->
      f

    (Expression.Global "Basics.<|", Expression.Global "Basics.identity":arg:args') ->
      simplifyApplication arg args'

    (Expression.Global "Basics.|>", arg:Expression.Global "Basics.identity":args') ->
      simplifyApplication arg args'

    (Expression.Global "List.::", element:Expression.List elements:args') ->
      simplifyApplication (Expression.List (element : elements)) args'

    (Expression.Global "String.join", [Expression.String separator, Expression.List args']) ->
      stringJoin separator args'

    (Expression.Global "String.concat", [Expression.List args']) ->
      stringConcat args'

    (Expression.Global "List.concat", [Expression.List args']) ->
      listConcat args'

    (Expression.Global "Basics.++", [arg1, arg2]) ->
      append arg1 arg2

    (Expression.Global _, _) ->
      Expression.apps expr args

    (Expression.App e1 e2, _) ->
      simplifyApplication e1 (simplifyExpression e2 : args)

    (Expression.Let e s, _)
      | freelyDuplicable e' ->
        simplifyApplication (instantiate1 e' s) args

      | length (Scope.bindings s) <= 1 ->
        simplifyApplication (instantiate1 e' s) args

      | otherwise ->
        Expression.apps
          (Expression.Let e' (simplifyScope s))
          args
      where
        e' =
          simplifyExpression e

    (Expression.Lam s, []) ->
      -- eta reduction (only for single-argument lambdas for now)
      case simplifyExpression $ fromScope s of
        Expression.App (unusedVar -> Just f) (Expression.Var (B ())) ->
          f

        e ->
          Expression.Lam $ toScope e

    (Expression.Lam s, arg:args') ->
      simplifyApplication (Expression.Let arg s) args'

    (Expression.Record fields, _) ->
      Expression.apps (Expression.Record $ fmap simplifyExpression <$> fields) args

    (Expression.Proj _, _) ->
      Expression.apps expr args

    (Expression.Case scrutinee branches, _) ->
      let
        scrutinee' =
          simplifyExpression scrutinee
      in
        case findMatchingBranch scrutinee' branches of
          Nothing ->
            case scrutinee' of
              Expression.Case innerScrutinee innerBranches ->
                simplifyApplication
                  (Expression.Case
                    innerScrutinee
                    [ (pat, toScope $ Expression.Case (fromScope branch) (second (fmap F) <$> branches))
                    | (pat, branch) <- innerBranches
                    ]
                  )
                  args

              _ ->
                Expression.apps
                  (Expression.Case scrutinee' $ fmap simplifyScope <$> branches)
                  args

          Just expr' ->
            simplifyApplication expr' args

    (Expression.List es, _) ->
      Expression.apps (Expression.List $ simplifyExpression <$> es) args

    (Expression.String _, _) ->
      Expression.apps expr args

    (Expression.Int _, _) ->
      Expression.apps expr args

    (Expression.Float _, _) ->
      Expression.apps expr args

simplifyScope
  :: Scope b Expression v
  -> Scope b Expression v
simplifyScope =
  toScope . simplifyExpression . fromScope

findMatchingBranch
  :: Eq b
  => Expression v
  -> [(Pattern b, Scope b Expression v)]
  -> Maybe (Expression v)
findMatchingBranch scrutinee branches =
  case branches of
    [] ->
      Nothing

    (pat, branch):branches' ->
      case match scrutinee pat of
        Nope ->
          findMatchingBranch scrutinee branches'

        Dunno ->
          Nothing

        Yep bindings ->
          Just $ Expression.lets bindings branch

data Match a
  = Nope
  | Dunno
  | Yep a

instance Semigroup a => Semigroup (Match a) where
  Nope <> _ =
    Nope

  _ <> Nope =
    Nope

  Dunno <> _ =
    Dunno

  _ <> Dunno =
    Dunno

  Yep a <> Yep b =
    Yep $ a <> b

instance Monoid a => Monoid (Match a) where
  mempty =
    Yep mempty

match
  :: Expression v
  -> Pattern b
  -> Match [(b, Expression v)]
match expr pat =
  case (expr, pat) of
    (_, Pattern.Var v) ->
      Yep [(v, expr)]

    (_, Pattern.Wildcard) ->
      mempty

    (_, Pattern.Con c2 pats) ->
      case Expression.appsView expr of
        (Expression.Global name, exprs)
          | name == c2 && length exprs == length pats ->
            fold $ zipWith match exprs pats

          | Name.isConstructor name ->
            Nope

        _ ->
          Dunno

    (Expression.List exprs, Pattern.List pats)
      | length exprs == length pats ->
        fold $ zipWith match exprs pats

    (Expression.List exprs@(_:_), _) ->
      match (foldr (\e1 e2 -> Expression.apps "List.::" [e1, e2]) (Expression.List []) exprs) pat

    (_, Pattern.List pats@(_:_)) ->
      match expr (foldr (\p1 p2 -> Pattern.Con "List.::" [p1, p2]) (Pattern.List []) pats)

    (_, Pattern.List []) ->
      case Expression.appsView expr of
        (Expression.Global name, _)
          | Name.isConstructor name ->
            Nope

        _ ->
            Dunno

    (Expression.String s1, Pattern.String s2)
      | s1 == s2 ->
        mempty

      | otherwise ->
        Nope

    (_, Pattern.String _) ->
      Dunno

    (Expression.Int i1, Pattern.Int i2)
      | i1 == i2 ->
        mempty

      | otherwise ->
        Nope

    (_, Pattern.Int _) ->
      Dunno

    (Expression.Float f1, Pattern.Float f2)
      | f1 == f2 ->
        mempty

      | otherwise ->
        Nope

    (_, Pattern.Float _) ->
      Dunno

freelyDuplicable :: Expression v -> Bool
freelyDuplicable expr =
  case expr of
    Expression.Var _ ->
      True

    Expression.Global _ ->
      True

    Expression.App {} ->
      False

    Expression.Let {} ->
      False

    Expression.Lam _ ->
      False

    Expression.Record [] ->
      True

    Expression.Record _ ->
      False

    Expression.Proj _ ->
      True

    Expression.Case {} ->
      False

    Expression.List [] ->
      True

    Expression.List _ ->
      False

    Expression.String s ->
      Text.length s < 10

    Expression.Int _ ->
      True

    Expression.Float _ ->
      True

unusedVar :: Traversable f => f (Var b a) -> Maybe (f a)
unusedVar =
  traverse $ unvar (const Nothing) pure

stringJoin :: Text -> [Expression v] -> Expression v
stringJoin separator args =
  case mergeAndJoinAdjacentLiterals args of
    [] ->
      Expression.String ""

    [arg] ->
      arg

    [arg1, arg2] ->
      stringConcat [arg1, Expression.String separator, arg2]

    args' ->
      Expression.apps "String.join" [Expression.String separator, Expression.List args']
  where
    mergeAndJoinAdjacentLiterals args' =
      case args' of
        [] ->
          []

        Expression.String s1:Expression.String s2:args'' ->
          mergeAndJoinAdjacentLiterals $ Expression.String (s1 <> separator <> s2) : args''

        arg:args'' ->
          arg:mergeAndJoinAdjacentLiterals args''

append :: Expression v -> Expression v -> Expression v
append arg1 arg2 =
  case mergeAdjacentLiterals $ unconcat =<< [arg1, arg2] of
    [] ->
      arg1 Expression.++ arg2

    [arg] ->
      arg

    [arg1', arg2'] ->
      arg1' Expression.++ arg2'

    args@(arg:args')
      | any isStringLiteral args ->
        Expression.App "String.concat" $ Expression.List args

      | any isListLiteral args ->
        Expression.App "List.concat" $ Expression.List args

      | otherwise ->
        foldl (Expression.++) arg args'
  where
    isStringLiteral expr =
      case expr of
        Expression.String _ ->
          True

        _ ->
          False

    isListLiteral expr =
      case expr of
        Expression.List _ ->
          True

        _ ->
          False

unconcat :: Expression v -> [Expression v]
unconcat expr =
  case Expression.appsView expr of
    (Expression.Global "String.concat", [Expression.List args]) ->
      unconcat =<< args

    (Expression.Global "List.concat", [Expression.List args]) ->
      unconcat =<< args

    (Expression.Global "Basics.++", [arg1, arg2]) ->
      unconcat =<< [arg1, arg2]

    _ ->
      [expr]

mergeAdjacentLiterals :: [Expression v] -> [Expression v]
mergeAdjacentLiterals args =
  case args of
    [] ->
      []

    Expression.String s1:Expression.String s2:args' ->
      mergeAdjacentLiterals $ Expression.String (s1 <> s2) : args'

    Expression.List l1:Expression.List l2:args' ->
      mergeAdjacentLiterals $ Expression.List (l1 <> l2) : args'

    arg:args' ->
      arg:mergeAdjacentLiterals args'

stringConcat :: [Expression v] -> Expression v
stringConcat args =
  case mergeAdjacentLiterals $ unconcat =<< args of
    [] ->
      Expression.String ""

    [arg] ->
      arg

    [arg1, arg2] ->
      arg1 Expression.++ arg2

    args' ->
      Expression.App "String.concat" $ Expression.List args'

listConcat :: [Expression v] -> Expression v
listConcat args =
  case mergeAdjacentLiterals $ unconcat =<< args of
    [] ->
      Expression.List []

    [arg] ->
      arg

    [arg1, arg2] ->
      arg1 Expression.++ arg2

    args' ->
      Expression.App "List.concat" $ Expression.List args'
