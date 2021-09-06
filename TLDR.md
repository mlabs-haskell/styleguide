# Introduction

This is a shortened version of the style guide, spelling out the rules, but not
the motivations. 

# Conventions

The words MUST, SHOULD, MUST NOT, SHOULD NOT and MAY are defined as per [RFC
2119][rfc-2119].

# Tools

## Compiler warning settings

The following warnings MUST be enabled for all builds of any project, or any
project component, in the `ghc-options` of the Cabal file:

* ``-Wall``
* ``-Wcompat``
* ``-Wincomplete-uni-patterns``
* ``-Wredundant-constraints``
* ``-Wmissing-export-lists``
* ``-Wmissing-deriving-strategies``
* ``-Werror``

Additionally, ``-Wincomplete-record-updates`` SHOULD be enabled for all builds
of any project, in the `ghc-options` of the Cabal file. The only exception is 
when this warning would be spuriously triggered by ``record-dot-preprocessor``, 
which occurs for definitions like this:

```haskell
data Foo = Bar {
   baz :: Int,
   quux :: String
   } | 
   Quux
```

Additionally, ``-Wredundant-constraints`` SHOULD be enabled for all builds of
any project, in the `ghc-options` of the Cabal file. Exceptions are allowed 
when the additional constraints are designed to ensure safety, rather than due 
to reliance on any method.

If a warning from this list is to be disabled, it MUST be disabled in the
narrowest possible scope; ideally, this SHOULD be a single module.

## Linting

Every source file MUST be free of warnings as produced by [HLint][hlint], using
the settings described in `.hlint.yaml`. A copy of such a file is provided in
this repository.

## Code formatting

Every source file MUST be formatted according to [Fourmolu][fourmolu], with the
following settings (as per its settings file):

* ``indentation: 2``
* ``comma-style: leading``
* ``record-brace-space: true``
* ``indent-wheres: true``
* ``diff-friendly-import-export: true``
* ``respectful: true``
* ``haddock-style: multi-line``
* ``newlines-between-decls: 1``

A copy of a configuration file with these settings is provided in this
repository.

Each source code line MUST be at most 100 characters wide, and SHOULD
be at most 80 characters wide.

# Code practices

## Naming

camelCase MUST be used for all non-type, non-data-constructor names; otherwise,
TitleCase MUST be used. Acronyms used as part of a naming identifier (such as 
'JSON', 'API', etc) SHOULD be downcased; thus ``repairJson`` and
``fromHttpService`` are correct. Exceptions are allowed for external libraries
(Aeson's ``parseJSON`` for example).

## Modules

All publically facing modules (namely, those which are not listed in
``other-modules`` in the Cabal file) MUST have explicit export lists.

All modules MUST use one of the following conventions for imports:

* ``import Foo (Baz, Bar, quux)``
* ``import qualified Foo as F``

Data types from qualified-imported modules SHOULD be imported unqualified by
themselves:

```haskell
import Data.Vector (Vector)
import qualified Data.Vector as Vector
```

The main exception is if such an import would cause a name clash:

```haskell
-- no way to import both of these without clashing the Vector type name
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as VStorable
```

The _sole_ exception is a 'hiding import' to replace part of the functionality
of ``Prelude``:

```haskell
-- replace the String-based readFile with a Text-based one
import Prelude hiding (readFile)
import Data.Text.IO (readFile)
```

Data constructors SHOULD be imported individually. For example, given the
following data type declaration:

```haskell
module Quux where

data Foo = Bar Int | Baz
```

Its corresponding import should be:

```haskell
import Quux (Foo, Bar, Baz)
```

For type class methods, the type class and its methods MUST be imported
as so:

```haskell
import Data.Aeson (FromJSON (fromJSON))
```

Qualified imports SHOULD use the entire module name (that is, the last component
of its hierarchical name) as the prefix. For example:

```haskell
import qualified Data.Vector as Vector
```

Exceptions are granted when:

* The import would cause a name clash anyway (such as different ``vector``
  modules); or
* We have to import a data type qualified as well.

Qualified imports of multiple modules MUST NOT be imported under the same name.
Thus, the following is wrong:

```haskell
import qualified Foo.Bar as Baz
import qualified Foo.Quux as Baz
```

## Plutus module import naming conventions

In addition to the general module import rules, we follow some conventions 
on how we import the Plutus API modules, allowing for some flexibility 
depending on the needs of a particular module.

Modules under the names `Plutus`, `Ledger` and `Plutus.V1.Ledger` SHOULD 
be imported qualified with their module name, as per the general module standards. 
An exception to this is `Plutus.V1.Ledger.Api`, where the `Ledger` name is preferred.

Some other exceptions to this are allowed where it may be more convenient to 
avoid longer qualified names.

For example:

```haskell
import Plutus.V1.Ledger.Slot qualified as Slot
import Plutus.V1.Ledger.Tx qualified as Tx
import Plutus.V1.Ledger.Api qualified as Ledger
import Ledger.Oracle qualified as Oracle
import Plutus.Contract qualified as Contract
```

In some cases it may be justified to use a shortened module name:

```haskell
import Plutus.V1.Ledger.AddressMap qualified as AddrMap
```

Modules under `PlutusTx` that are extensions to `PlutusTx.Prelude` MAY be 
imported unqualified when it is reasonable to do so. 

The `Plutus.V1.Ledger.Api` module SHOULD be avoided in favour of more 
specific modules where possible. For example, we should avoid:

```haskell
import Plutus.V1.Ledger.Api qualified as Ledger
```

In favour of:

```haskell
import Plutus.V1.Ledger.Scripts qualified as Scripts
```

## LANGUAGE pragmata

The following pragmata MUST be enabled at project level (that is, in
the Cabal file):

* ``BangPatterns``
* ``BinaryLiterals``
* ``ConstraintKinds``
* ``DataKinds``
* ``DeriveFunctor``
* ``DeriveGeneric``
* ``DeriveTraversable``
* ``DerivingStrategies``
* ``DerivingVia``
* ``DuplicateRecordFields``
* ``EmptyCase``
* ``FlexibleContexts``
* ``FlexibleInstances``
* ``GADTs``
* ``GeneralizedNewtypeDeriving``
* ``HexFloatLiterals``
* ``InstanceSigs``
* ``ImportQualifiedPost``
* ``KindSignatures``
* ``LambdaCase``
* ``MultiParamTypeClasses``
* ``NoImplicitPrelude``
* ``NumericUnderscores``
* ``OverloadedStrings``
* ``ScopedTypeVariables``
* ``StandaloneDeriving``
* ``TupleSections``
* ``TypeApplications``
* ``TypeOperators``
* ``TypeSynonymInstances``
* ``UndecidableInstances``

Any other LANGUAGE pragmata MUST be enabled per-file. All language pragmata MUST
be at the top of the source file, written as ``{-# LANGUAGE PragmaName #-}``.

Furthermore, the following pragmata MUST NOT be used, or enabled, anywhere:

* ``DeriveDataTypeable``
* ``DeriveFoldable``
* ``PartialTypeSignatures``
* ``PostfixOperators``

## ``record-dot-preprocessor``

The GHC plugin from ``record-dot-preprocessor`` SHOULD be enabled globally. 

## Prelude

The ``PlutusTx.Prelude`` MUST be used. A 'hiding import' to remove functionality
we want to replace SHOULD be used when necessary. If functionality from the
``Prelude`` in ``base`` is needed, it SHOULD be imported qualified. Other
preludes MUST NOT be used.

## Versioning

A project MUST use the [PVP][pvp]. Two, and only two, version numbers MUST be
used: a major version and a minor version.

## Documentation

Every publically-exported definition MUST have a Haddock comment, detailing its
purpose. If a definition is a function, it SHOULD also have examples of use
using [Bird tracks][bird-tracks]. The Haddock for a publically-exported
definition SHOULD also provide an explanation of any caveats, complexities of
its use, or common issues a user is likely to encounter. 

If the code project is a library, these Haddock comments SHOULD carry an
[``@since``][haddock-since] annotation, stating what version of the library they
were introduced in, or the last version where their functionality or type
signature changed.

For type classes, their laws MUST be documented using a Haddock comment.

## Type and kind signatures

All module-level definitions, as well as ``where``-binds, MUST have explicit type
signatures. Type variables MUST have an explicit ``forall`` scoping them, and
all type variables MUST have explicit kind signatures. Thus, the following is
wrong:

```haskell
data Foo a = Bar | Baz [a]

quux :: (Monoid m) => [m] -> m -> m
```

Instead, write it like this:

```haskell
data Foo (a :: Type) = Bar | Baz [a]

quux :: forall (m :: Type) . (Monoid m) => [m] -> m -> m
```

Each explicit type signature MUST correspond to one definition only. Thus, the
following is wrong:

```haskell
bar :: Int
baz :: Int
(bar, baz) = someOtherFunction someOtherValue
```

Instead, write it like this:

```haskell
bar :: Int
bar = fst . someOtherFunction $ someOtherValue

baz :: Int
baz = snd . someOtherFunction $ someOtherValue
```

## Other

Lists SHOULD NOT be field values of types; this extends to ``String``s. Instead,
``Vector``s (``Text``s) SHOULD be used, unless a more appropriate structure exists. 
On-chain code, due to a lack of alternatives, is one place lists can be used as
field values of types.

Partial functions MUST NOT be defined. Partial functions SHOULD NOT be used
except to ensure that another function is total (and the type system cannot be
used to prove it). 

Derivations MUST use an explicit [strategy][deriving-strategies]. Thus, the 
following is wrong:

```haskell
newtype Foo = Foo (Bar Int)
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
```

Instead, write it like this:

```haskell
newtype Foo = Foo (Bar Int)
    deriving stock (Generic)
    deriving newtype (Eq, Show)
    deriving anyclass (FromJSON, ToJSON)
```

Deriving via SHOULD be preferred to newtype derivation, especially where the
underlying type representation could change significantly.

``type`` SHOULD NOT be used. The only acceptable case is abbreviation of large
type-level computations. In particular, ``type`` MUST NOT be used to create an
abstraction boundary. 

# Design practices

## Parse, don't validate

[Boolean blindness][boolean-blindness] SHOULD NOT be used in the design of any
function or API. Returning more meaningful data SHOULD be the preferred choice.
The general principle of ['parse, don't validate'][parse-dont-validate] SHOULD
guide design and implementation.

## No multi-parameter type-classes without functional dependencies

Any multi-parameter type class MUST have a functional dependency restricting its
relation to a one-to-many at most. In cases of true many-to-many relationships,
type classes MUST NOT be used as a solution to the problem.

## Type classes must have laws

Any type class not imported from an external dependency MUST have laws. These
laws MUST be documented in a Haddock comment on the type class definition, and
all instances MUST follow these laws.

[pvp]: https://pvp.haskell.org/
[policeman]: https://hackage.haskell.org/package/policeman
[haddock-since]: https://haskell-haddock.readthedocs.io/en/latest/markup.html#since
[bird-tracks]: https://haskell-haddock.readthedocs.io/en/latest/markup.html#code-blocks
[hedgehog-classes]: http://hackage.haskell.org/package/hedgehog-classes
[hspec-hedgehog]: http://hackage.haskell.org/package/hspec-hedgehog
[property-based-testing]: https://dl.acm.org/doi/abs/10.1145/1988042.1988046
[hedgehog]: http://hackage.haskell.org/package/hedgehog
[deriving-strategies]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/deriving-strategies
[functor-parametricity]: https://www.schoolofhaskell.com/user/edwardk/snippets/fmap
[alexis-king-options]: https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/#warning-flags-for-a-safe-build
[hlint]: http://hackage.haskell.org/package/hlint
[fourmolu]: http://hackage.haskell.org/package/fourmolu
[rfc-2119]: https://tools.ietf.org/html/rfc2119
[boolean-blindness]: http://dev.stephendiehl.com/hask/#boolean-blindness
[parse-dont-validate]: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
[hspec]: http://hackage.haskell.org/package/hspec
[rdp]: https://hackage.haskell.org/package/record-dot-preprocessor
[rdp-issue]: https://github.com/ghc-proposals/ghc-proposals/pull/282
