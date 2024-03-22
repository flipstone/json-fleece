{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.ReferenceOneOfInsideOneOf
  ( ReferenceOneOfInsideOneOf(..)
  , referenceOneOfInsideOneOfSchema
  ) where

import Fleece.Core ((#|))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified Shrubbery as Shrubbery
import qualified TestCases.Types.ReferenceOneOf as ReferenceOneOf
import qualified TestCases.Types.TopLevelOneOfOneOption as TopLevelOneOfOneOption

newtype ReferenceOneOfInsideOneOf = ReferenceOneOfInsideOneOf (Shrubbery.Union
  '[ ReferenceOneOf.ReferenceOneOf
   , TopLevelOneOfOneOption.TopLevelOneOfOneOption
   , [TopLevelOneOfOneOption.TopLevelOneOfOneOption]
   ])
  deriving (Show, Eq)

referenceOneOfInsideOneOfSchema :: FC.Fleece schema => schema ReferenceOneOfInsideOneOf
referenceOneOfInsideOneOfSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.ReferenceOneOfInsideOneOf" "ReferenceOneOfInsideOneOf") $
      FC.unionMember ReferenceOneOf.referenceOneOfSchema
        #| FC.unionMember TopLevelOneOfOneOption.topLevelOneOfOneOptionSchema
        #| FC.unionMember (FC.list TopLevelOneOfOneOption.topLevelOneOfOneOptionSchema)