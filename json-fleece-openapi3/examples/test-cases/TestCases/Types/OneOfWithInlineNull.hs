{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Types.OneOfWithInlineNull
  ( OneOfWithInlineNull(..)
  , oneOfWithInlineNullSchema
  ) where

import Fleece.Core ((#|))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified Shrubbery as Shrubbery

newtype OneOfWithInlineNull = OneOfWithInlineNull (Shrubbery.Union
  '[ FC.Null
   , [FC.Null]
   ])
  deriving (Show, Eq)

oneOfWithInlineNullSchema :: FC.Fleece t => FC.Schema t OneOfWithInlineNull
oneOfWithInlineNullSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.OneOfWithInlineNull" "OneOfWithInlineNull") $
      FC.unionMember FC.null
        #| FC.unionMember (FC.list FC.null)