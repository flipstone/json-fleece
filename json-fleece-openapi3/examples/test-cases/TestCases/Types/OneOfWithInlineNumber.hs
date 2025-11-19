{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Types.OneOfWithInlineNumber
  ( OneOfWithInlineNumber(..)
  , oneOfWithInlineNumberSchema
  ) where

import qualified Data.Scientific as Sci
import Fleece.Core ((#|))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified Shrubbery as Shrubbery

newtype OneOfWithInlineNumber = OneOfWithInlineNumber (Shrubbery.Union
  '[ Sci.Scientific
   , [Sci.Scientific]
   ])
  deriving (Show, Eq)

oneOfWithInlineNumberSchema :: FC.Fleece schema => schema OneOfWithInlineNumber
oneOfWithInlineNumberSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.OneOfWithInlineNumber" "OneOfWithInlineNumber") $
      FC.unionMember FC.number
        #| FC.unionMember (FC.list FC.number)