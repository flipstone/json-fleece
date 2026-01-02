{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Types.OneOfWithInlineEnum
  ( OneOfWithInlineEnum(..)
  , oneOfWithInlineEnumSchema
  ) where

import Fleece.Core ((#|))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified Shrubbery as Shrubbery
import qualified TestCases.Operations.OneOfWithInlineEnum.Option1 as Option1
import qualified TestCases.Operations.OneOfWithInlineEnum.Option2 as Option2

newtype OneOfWithInlineEnum = OneOfWithInlineEnum (Shrubbery.Union
  '[ Option1.Option1
   , Option2.Option2
   ])
  deriving (Show, Eq)

oneOfWithInlineEnumSchema :: FC.Fleece t => FC.Schema t OneOfWithInlineEnum
oneOfWithInlineEnumSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.OneOfWithInlineEnum" "OneOfWithInlineEnum") $
      FC.unionMember Option1.option1Schema
        #| FC.unionMember Option2.option2Schema