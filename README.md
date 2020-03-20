# elm-syntax [![Build Status](https://travis-ci.com/folq/elm-syntax.svg?branch=master)](https://travis-ci.com/folq/elm-syntax) [![Hackage](https://img.shields.io/hackage/v/elm-syntax.svg)](https://hackage.haskell.org/package/elm-syntax)

A library for generating Elm syntax from Haskell in a scope-safe way.

This library has two main parts:

* It defines abstract syntax trees (ASTs) for Elm definitions, expressions, patterns,
and types, using the [bound library](http://hackage.haskell.org/package/bound)
for scope-safe local names.
* It defines pretty printers from the ASTs to syntactically correct Elm code.
