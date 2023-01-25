{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
module Language.Elm.Pretty
  (
  -- * Modules
    modules
  , module_
  -- * Environments
  , Environment(..)
  , emptyEnvironment
  , extend
  -- * Pretty-printing names
  , local
  , field
  , constructor
  , moduleName
  , qualified
  -- * Pretty-printing definitions
  , definition
  -- * Pretty-printing expressions
  , expression
  -- * Pretty-printing pattern
  , pattern
  -- * Pretty-printing types
  , type_
  ) where

import qualified Bound
import qualified Bound.Var as Bound
import Data.Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List (sort, intersperse)
import Data.Maybe (isNothing)
import Data.String
import Data.Text.Prettyprint.Doc
import Data.Void

import Language.Elm.Definition (Definition)
import qualified Language.Elm.Definition as Definition
import Language.Elm.Expression (Expression)
import qualified Language.Elm.Expression as Expression
import qualified Language.Elm.Name as Name
import Language.Elm.Pattern (Pattern)
import qualified Language.Elm.Pattern as Pattern
import Language.Elm.Type (Type)
import qualified Language.Elm.Type as Type

-------------------------------------------------------------------------------
-- * Modules

-- | Group the given definitions by their defining module, and generate an Elm
-- module for each group.
modules :: [Definition] -> HashMap Name.Module (Doc ann)
modules defs =
  let
    defsByModule =
      foldl'
        (HashMap.unionWith (<>))
        mempty
        [ HashMap.singleton m [def]
        | def <- defs
        , let
            (Name.Qualified m _) =
              Definition.name def
        ]
  in
  HashMap.mapWithKey module_ defsByModule

-- | Generate an Elm module containing the given definitions.
module_ :: Name.Module -> [Definition] -> Doc ann
module_ mname defs =
  let
    usedNames =
      HashSet.fromList
        [ Name.Local name
        | Name.Qualified _ name <- Definition.name <$> defs
        ]

    env =
      (emptyEnvironment mname)
        { freshLocals = filter (not . (`HashSet.member` usedNames)) $ freshLocals (emptyEnvironment mname)
        }

    imports =
      sort $
      HashSet.toList $
      flip HashSet.difference defaultImports $
      HashSet.filter (/= mname) $
      HashSet.map (\(Name.Qualified m _) -> m) $
      HashSet.filter (isNothing . defaultImport) $
      foldMap (Definition.foldMapGlobals HashSet.singleton) defs

    exports =
      indent 4 $
      (<> line <> ")") $
      vsep $
      zipWith (<>) ("( " : repeat ", ") $
      map export defs

    export = \case
      Definition.Constant (Name.Qualified _ name) _ _ _ -> pretty name
      Definition.Type (Name.Qualified _ name) _ _ -> pretty name <> "(..)"
      Definition.Alias (Name.Qualified _ name) _ _ -> pretty name

  in
  "module" <+> moduleName mname <+> "exposing" <> line <> exports <> line <> line <>
  mconcat ["import" <+> moduleName import_ <> line | import_ <- imports] <> line <> line <>
  mconcat (intersperse (line <> line <> line) [definition env def | def <- defs])

defaultImports :: HashSet Name.Module
defaultImports =
  HashSet.fromList
    [ ["Basics"]
    , ["List"]
    , ["Maybe"]
    , ["Result"]
    , ["String"]
    , ["Char"]
    , ["Tuple"]
    , ["Debug"]
    , ["Platform"]
    , ["Cmd"]
    , ["Sub"]
    ]

-------------------------------------------------------------------------------
-- * Environments

-- | A pretty-printing environment with local variables in @v@.
data Environment v = Environment
  { locals :: v -> Name.Local
  , freshLocals :: [Name.Local]
  , currentModule :: Name.Module
  }

emptyEnvironment :: Name.Module -> Environment Void
emptyEnvironment m = Environment
  { locals = absurd
  , freshLocals = (fromString . pure <$> ['a'..'z']) ++ [fromString $ [x] <> show n | x <- ['a'..'z'], n <- [(0 :: Int)..]]
  , currentModule = m
  }

extend :: Environment v -> (Environment (Bound.Var () v), Name.Local)
extend env =
  case freshLocals env of
    [] ->
      error "Language.Elm.Pretty no locals"

    fresh:freshLocals' ->
      ( env
        { locals = Bound.unvar (\() -> fresh) (locals env)
        , freshLocals = freshLocals'
        }
      , fresh
      )

extendPat :: Environment v -> Pattern Int -> Environment (Bound.Var Int v)
extendPat env pat =
  let
    occurrencesSet =
      foldMap HashSet.singleton pat

    occurrences =
      HashSet.toList occurrencesSet
  in
  extendMany env occurrences

extendMany :: Environment v -> [Int] -> Environment (Bound.Var Int v)
extendMany env occurrences =
  let
    bindings =
      HashMap.fromList $
        zip occurrences $ freshLocals env

    freshLocals' =
      drop (length occurrences) $ freshLocals env

    lookupVar i =
      case HashMap.lookup i bindings of
        Nothing ->
          error "Language.Elm.Pretty unbound pattern variable"

        Just v ->
          v
  in
  env
    { locals = Bound.unvar lookupVar (locals env)
    , freshLocals = freshLocals'
    }

-------------------------------------------------------------------------------
-- * Pretty-printing names

local :: Name.Local -> Doc ann
local (Name.Local l) =
  pretty l

field :: Name.Field -> Doc ann
field (Name.Field f) =
  pretty f

constructor :: Name.Constructor -> Doc ann
constructor (Name.Constructor c) =
  pretty c

moduleName :: Name.Module -> Doc ann
moduleName ms =
  mconcat (intersperse dot $ pretty <$> ms)

qualified :: Environment v -> Name.Qualified -> Doc ann
qualified env name@(Name.Qualified ms l) =
  case defaultImport name of
    Nothing
      | ms == currentModule env ->
        pretty l

      | otherwise ->
        case ms of
          [] ->
            pretty l

          _ ->
            moduleName ms <> dot <> pretty l

    Just l' ->
      local l'

defaultImport :: Name.Qualified -> Maybe Name.Local
defaultImport qname =
  case qname of
    Name.Qualified ["Basics"] name ->
      Just $ Name.Local name

    "Cmd.Cmd" ->
      Just "Cmd"

    "List.List" ->
      Just "List"

    "List.::" ->
      Just "::"

    "Maybe.Maybe" ->
      Just "Maybe"

    "Maybe.Nothing" ->
      Just "Nothing"

    "Maybe.Just" ->
      Just "Just"

    "Result.Result" ->
      Just "Result"

    "Result.Ok" ->
      Just "Ok"

    "Result.Err" ->
      Just "Err"

    "String.String" ->
      Just "String"

    "Char.Char" ->
      Just "Char"

    _ -> Nothing

fixity :: Name.Qualified -> Maybe (Int, Int, Int)
fixity qname =
  case qname of
    "Basics.>>" ->
      leftAssoc 9

    "Basics.<<" ->
      rightAssoc 9

    "Basics.^" ->
      rightAssoc 8

    "Basics.*" ->
      leftAssoc 7

    "Basics./" ->
      leftAssoc 7

    "Basics.//" ->
      leftAssoc 7

    "Basics.%" ->
      leftAssoc 7

    "Basics.+" ->
      leftAssoc 6

    "Basics.-" ->
      leftAssoc 6

    "Parser.|=" ->
      leftAssoc 5

    Name.Qualified ["Parser"] "|." ->
      leftAssoc 6

    "Basics.++" ->
      rightAssoc 5

    "List.::" ->
      rightAssoc 5

    "Basics.==" ->
      noneAssoc 4

    "Basics./=" ->
      noneAssoc 4

    "Basics.<" ->
      noneAssoc 4

    "Basics.>" ->
      noneAssoc 4

    "Basics.<=" ->
      noneAssoc 4

    "Basics.>=" ->
      noneAssoc 4

    "Basics.&&" ->
      rightAssoc 3

    "Basics.||" ->
      leftAssoc 3

    "Basics.|>" ->
      leftAssoc 0

    "Basics.<|" ->
      rightAssoc 0

    "Basics.," ->
      Just (0, -1, 0)

    _ ->
      Nothing

  where
    leftAssoc n =
      Just (n, n, n + 1)

    rightAssoc n =
      Just (n, n, n + 1)

    noneAssoc n =
      Just (n + 1, n, n + 1)

twoLineOperator :: Name.Qualified -> Bool
twoLineOperator qname =
  case qname of
    "Basics.>>" ->
      True

    "Basics.<<" ->
      True

    "Basics.|>" ->
      True

    "Basics.<|" ->
      True

    _ ->
      False

-------------------------------------------------------------------------------
-- * Definitions

definition :: Environment Void -> Definition -> Doc ann
definition env def =
  case def of
    Definition.Constant (Name.Qualified _ name) numTypeParams t e ->
      let
        typeParams =
          [0..numTypeParams - 1]

        typeEnv =
          extendMany env typeParams

        (names, body) =
          lambdas env e
      in
      pretty name <+> ":" <+> nest 4 (type_ typeEnv 0 $ Bound.fromScope t) <> line <>
      (case names of
        [] ->
          pretty name <+> "="

        _ ->
          pretty name <+> hsep (local <$> names) <+> "=") <>
      line <> indent 4 body

    Definition.Type (Name.Qualified _ name) numParams constrs ->
      let
        params =
          [0..numParams - 1]

        env' =
          extendMany env params
      in
      "type" <+> pretty name <+> hsep (local . locals env' . Bound.B <$> params) <> line <>
        indent 4 ("=" <+>
          mconcat
            (intersperse (line <> "| ")
              [constructor c <+> hsep (type_ env' (appPrec + 1) . Bound.fromScope <$> ts) | (c, ts) <- constrs]))

    Definition.Alias (Name.Qualified _ name) numParams t ->
      let
        params =
          [0..numParams - 1]

        env' =
          extendMany env params
      in
      "type alias" <+> pretty name <+> hsep (local . locals env' . Bound.B <$> params) <+> "=" <> line <>
      indent 4 (type_ env' 0 $ Bound.fromScope t)

-------------------------------------------------------------------------------
-- * Expressions

expression :: Environment v -> Int -> Expression v -> Doc ann
expression env prec expr =
  case expr of
    Expression.Var var ->
      local $ locals env var

    (Expression.appsView -> (Expression.Proj f, arg:args)) ->
      atomApps (expression env) prec (expression env projPrec arg <> dot <> field f) args

    (Expression.appsView -> (Expression.Global qname@(Name.Qualified _ name), args)) ->
      case fixity qname of
        Nothing ->
          atomApps (expression env) prec (qualified env qname) args

        Just (leftPrec, opPrec, rightPrec) ->
          case args of
            [arg1, arg2] ->
              parensWhen (prec > opPrec) $
                expression env leftPrec arg1 <+> pretty name <>
                (if twoLineOperator qname then line else space) <>
                expression env rightPrec arg2

            arg1:arg2:args' ->
              apps (expression env) prec (Expression.apps (Expression.Global qname) [arg1, arg2]) args'

            _ ->
              atomApps (expression env) prec (parens $ pretty name) args

    (Expression.appsView -> (fun, args@(_:_))) ->
      apps (expression env) prec fun args

    Expression.Global _ ->
      error "Language.Elm.Pretty expression Global"

    Expression.App {} ->
      error "Language.Elm.Pretty expression App"

    Expression.Let {} ->
      parensWhen (prec > letPrec) $
        let
          (bindings, body) =
            lets env expr
        in
        line <> "let"
        <> line <> indent 4 (mconcat $ intersperse (line <> line) bindings)
        <> line <> "in"
        <> line <> body

    Expression.Lam {} ->
      parensWhen (prec > lamPrec) $
        let
          (names, body) =
            lambdas env expr
        in
        "\\" <> hsep (local <$> names) <+> "->" <+> body

    Expression.Record fields ->
      encloseSep "{ " " }" ", "
        [ field f <+> "=" <+> expression env 0 expr'
        | (f, expr') <- fields
        ]

    Expression.Proj f ->
      "." <> field f

    Expression.Case bool_
      [ (Pattern.Con "Basics.True" [], unusedScope -> Just true)
      , (Pattern.Con "Basics.False" [], unusedScope -> Just false)
      ] ->
      parensWhen (prec > ifPrec) $
        "if" <+> expression env 0 bool_ <+> "then" <> line <>
          indent 4 (expression env 0 true) <> line <>
        line <>
        "else" <> line <>
          indent 4 (expression env 0 false)

    Expression.Case expr' branches ->
      parensWhen (prec > casePrec) $
        "case" <+> expression env 0 expr' <+> "of" <> line <>
        indent 4
        (
        mconcat $
        intersperse (line <> line) $
          [ pattern env' 0 pat <+> "->" <> line <> indent 4 (expression env' 0 (Bound.fromScope scope))
          | (pat, scope) <- branches
          , let
              env' =
                extendPat env pat
          ]
        )

    Expression.List exprs ->
      list $ expression env 0 <$> exprs

    Expression.String s ->
      "\"" <> pretty s <> "\""

    Expression.Int i ->
      pretty i

    Expression.Float f ->
      pretty f

lets :: Environment v -> Expression v -> ([Doc ann], Doc ann)
lets env expr =
  case expr of
    Expression.Let expr' scope ->
      let
        (env', name) =
          extend env

        (bindings, body) =
          lets env' (Bound.fromScope scope)

        binding =
          local name <+> "="
            <> line <> indent 4 (expression env 0 expr')

      in
      (binding : bindings , body)

    _ ->
      ([], expression env letPrec expr)

lambdas :: Environment v -> Expression v -> ([Name.Local], Doc ann)
lambdas env expr =
  case expr of
    Expression.Lam scope ->
      let
        (env', name) =
          extend env

        (names, body) =
          lambdas env' (Bound.fromScope scope)
      in
      (name : names, body)

    _ ->
      ([], expression env lamPrec expr)

-------------------------------------------------------------------------------
-- * Patterns

pattern :: Environment (Bound.Var Int v) -> Int -> Pattern Int -> Doc ann
pattern env prec pat =
  case pat of
    Pattern.Var var ->
      local $ locals env (Bound.B var)

    Pattern.Wildcard ->
      "_"

    Pattern.Con con [] ->
      qualified env con

    Pattern.Con con@(Name.Qualified _ name) pats ->
      case fixity con of
        Nothing ->
          parensWhen (prec > appPrec) $
            qualified env con <+> hsep (pattern env (appPrec + 1) <$> pats)

        Just (leftPrec, opPrec, rightPrec) ->
          case pats of
            [pat1, pat2] ->
              parensWhen (prec > opPrec) $
                pattern env leftPrec pat1 <+> pretty name <>
                (if twoLineOperator con then line else space) <>
                pattern env rightPrec pat2

            pat1:pat2:pats' ->
              apps (pattern env) prec (Pattern.Con con [pat1, pat2]) pats'

            _ ->
              parensWhen (prec > appPrec) $
                qualified env con <+> hsep (pattern env (appPrec + 1) <$> pats)

    Pattern.List pats ->
      list $ pattern env 0 <$> pats

    Pattern.String s ->
      "\"" <> pretty s <> "\""

    Pattern.Int i ->
      pretty i

    Pattern.Float f ->
      pretty f

-------------------------------------------------------------------------------
-- * Types

type_ :: Environment v -> Int -> Type v -> Doc ann
type_ env prec t =
  case t of
    Type.Var var ->
      local $ locals env var

    (Type.appsView -> (Type.Global qname@(Name.Qualified _ name), args)) ->
      case fixity qname of
        Nothing ->
          atomApps (type_ env) prec (qualified env qname) args

        Just (leftPrec, opPrec, rightPrec) ->
          case args of
            [arg1, arg2] ->
              parensWhen (prec > opPrec) $
                type_ env leftPrec arg1 <+> pretty name <>
                (if twoLineOperator qname then line else space) <>
                type_ env rightPrec arg2

            arg1:arg2:args' ->
              apps (type_ env) prec (Type.apps (Type.Global qname) [arg1, arg2]) args'

            _ ->
              atomApps (type_ env) prec (parens $ pretty name) args

    (Type.appsView -> (fun, args@(_:_))) ->
      apps (type_ env) prec fun args

    Type.Global _ ->
      error "Language.Elm.Pretty type_ Global"

    Type.App {} ->
      error "Language.Elm.Pretty type_ App"

    Type.Fun t1 t2 ->
      parensWhen (prec > funPrec) $
        type_ env (funPrec + 1) t1 <+> "->" <+> type_ env funPrec t2

    Type.Record fields ->
      encloseSep "{ " " }" ", "
        [ field f <+> ":" <+> type_ env 0 type'
        | (f, type') <- fields
        ]

-------------------------------------------------------------------------------
-- Utils

apps :: (Int -> a -> Doc ann) -> Int -> a -> [a] -> Doc ann
apps f prec fun args =
  case args of
    [] ->
      f prec fun

    _ ->
      parensWhen (prec > appPrec) $
        f appPrec fun <+> hsep (f (appPrec + 1) <$> args)

atomApps :: (Int -> a -> Doc ann) -> Int -> Doc ann -> [a] -> Doc ann
atomApps f prec fun args =
  case args of
    [] ->
      fun

    _ ->
      parensWhen (prec > appPrec) $
        fun <+> hsep (f (appPrec + 1) <$> args)

parensWhen :: Bool -> Doc ann -> Doc ann
parensWhen b =
  if b then
    parens

  else
    id

appPrec, letPrec, lamPrec, casePrec, ifPrec, funPrec, projPrec :: Int
appPrec = 10
letPrec = 0
lamPrec = 0
casePrec = 0
ifPrec = 0
funPrec = 0
projPrec = 11

unusedScope :: (Monad f, Traversable f) => Bound.Scope b f a -> Maybe (f a)
unusedScope =
  traverse (Bound.unvar (const Nothing) pure) . Bound.fromScope
