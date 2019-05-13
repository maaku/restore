{-# OPTIONS_GHC -fno-warn-orphans #-}
module GenericsBenchTypes where

import Cabal24
import Generics.Deriving.Instances ()
import Data.Restore

instance Restore Benchmark
instance Restore BenchmarkInterface
instance Restore BenchmarkType
instance Restore BuildInfo
instance Restore BuildType
instance Restore CompilerFlavor
instance Restore Dependency
instance Restore Executable
instance Restore Extension
instance Restore FlagName
instance Restore KnownExtension
instance Restore Language
instance Restore Library
instance Restore License
instance Restore ModuleName
instance Restore ModuleReexport
instance Restore ModuleRenaming
instance Restore PackageDescription
instance Restore PackageIdentifier
instance Restore PackageName
instance Restore RepoKind
instance Restore RepoType
instance Restore SetupBuildInfo
instance Restore SourceRepo
instance Restore TestSuite
instance Restore TestSuiteInterface
instance Restore TestType
instance Restore VersionRange
