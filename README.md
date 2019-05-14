# restore package #

[![Hackage version](https://img.shields.io/hackage/v/restore.svg?label=Hackage)](https://hackage.haskell.org/package/restore)
[![BSD3 license](https://img.shields.io/badge/license-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![Stackage Lts](https://stackage.org/package/restore/badge/lts)](https://stackage.org/lts/package/restore)
[![Stackage Nightly](https://stackage.org/package/restore/badge/nightly)](https://stackage.org/nightly/package/restore)
[![Build Status](https://api.travis-ci.org/maaku/restore.png?branch=master)](http://travis-ci.org/maaku/restore)

*Efficient, pure binary serialisation using lazy ByteStrings.*

The ``restore`` package provides Data.Restore, containing the Restore class,
and associated methods, for serialising values to and from lazy
ByteStrings.
A key feature of ``restore`` is that the interface is both pure, and efficient.

## Installing restore from Hackage ##

``restore`` is part of The Glasgow Haskell Compiler (GHC) and therefore if you
have either GHC or [The Haskell Platform](http://www.haskell.org/platform/)
installed, you already have ``restore``.

More recent versions of ``restore`` than you might have installed may be
available. You can use ``cabal-install`` to install a later version from
[Hackage](http://hackage.haskell.org/package/restore).

    $ cabal update
    $ cabal install restore

## Building restore ##

``restore`` comes with both a test suite and a set of benchmarks.
While developing, you probably want to enable both.
Here's how to get the latest version of the repository, configure and build.

    $ git clone git@github.com:maaku/restore.git
    $ cd restore
    $ cabal update
    $ cabal configure --enable-tests --enable-benchmarks
    $ cabal build

Run the test suite.

    $ cabal test

## Using restore ##

First:

    import Data.Restore

and then write an instance of Restore for the type you wish to serialise.
An example doing exactly this can be found in the Data.Restore module.
You can also use the Data.Restore.Builder module to efficiently build
lazy bytestrings using the ``Builder`` monoid. Or, alternatively, the
Data.Restore.Get and Data.Restore.Put to serialize/deserialize using
the ``Get`` and ``Put`` monads.

More information in the haddock documentation.

## Deriving restore instances using GHC's Generic ##

Beginning with GHC 7.2, it is possible to use binary serialization without
writing any instance boilerplate code.

```haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.Restore
import GHC.Generics (Generic)

data Foo = Foo deriving (Generic)

-- GHC will automatically fill out the instance
instance Restore Foo
```

## Contributors ##

* Lennart Kolmodin
* Duncan Coutts
* Don Stewart
* Spencer Janssen
* David Himmelstrup
* Björn Bringert
* Ross Paterson
* Einar Karttunen
* John Meacham
* Ulf Norell
* Tomasz Zielonka
* Stefan Karrmann
* Bryan O'Sullivan
* Bas van Dijk
* Florian Weimer

For a full list of contributors, see
[here](https://github.com/maaku/restore/graphs/contributors).
