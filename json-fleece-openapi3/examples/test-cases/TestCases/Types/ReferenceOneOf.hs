{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.ReferenceOneOf
  ( ReferenceOneOf(..)
  , referenceOneOfSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.TopLevelOneOfOneOption as TopLevelOneOfOneOption

data ReferenceOneOf = ReferenceOneOf
  { oneOfRef :: Maybe TopLevelOneOfOneOption.TopLevelOneOfOneOption
  }
  deriving (Eq, Show)

referenceOneOfSchema :: FC.Fleece schema => schema ReferenceOneOf
referenceOneOfSchema =
  FC.object $
    FC.constructor ReferenceOneOf
      #+ FC.optional "oneOfRef" oneOfRef TopLevelOneOfOneOption.topLevelOneOfOneOptionSchema