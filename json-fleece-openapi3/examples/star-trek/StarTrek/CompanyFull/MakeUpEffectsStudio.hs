{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyFull.MakeUpEffectsStudio
  ( MakeUpEffectsStudio(..)
  , makeUpEffectsStudioSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype MakeUpEffectsStudio = MakeUpEffectsStudio Bool
  deriving (Show, Eq)

makeUpEffectsStudioSchema :: FC.Fleece schema => schema MakeUpEffectsStudio
makeUpEffectsStudioSchema =
  FC.coerceSchema FC.boolean