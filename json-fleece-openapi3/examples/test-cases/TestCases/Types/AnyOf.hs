{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Types.AnyOf
  ( AnyOf(..)
  , anyOfSchema
  ) where

import Fleece.Core ((#|))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified Shrubbery as Shrubbery
import qualified TestCases.Operations.AnyOf.Option3 as Option3
import qualified TestCases.Types.Bar as Bar
import qualified TestCases.Types.Baz as Baz
import qualified TestCases.Types.Foo as Foo

newtype AnyOf = AnyOf (Shrubbery.Union
  '[ Foo.Foo
   , Bar.Bar
   , Option3.Option3
   , Baz.Baz
   ])
  deriving (Show, Eq)

anyOfSchema :: FC.Fleece schema => schema AnyOf
anyOfSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.AnyOf" "AnyOf") $
      FC.unionMember Foo.fooSchema
        #| FC.unionMember Bar.barSchema
        #| FC.unionMember Option3.option3Schema
        #| FC.unionMember Baz.bazSchema