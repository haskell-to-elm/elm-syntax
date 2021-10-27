# Changelog for elm-syntax

## 0.3.2.0

- The `Monad` instance for `Expression` has been redefined, leading to considerably faster performance. (see [#4](https://github.com/folq/elm-syntax/pull/4))

## 0.3.1.0

- Simplify record projections on known records
- Write explicit export lists
- Fix build with GHC 9.0. (see [#2](https://github.com/folq/elm-syntax/pull/2))

## 0.3.0.0

- Add support for parameterised types
- Handle Basics.() in isConstructor check
- Simplify case-of-case expressions

## 0.2.0.0

- Add simplification module with some useful optimisations


## 0.1.0.0

- Initial release
