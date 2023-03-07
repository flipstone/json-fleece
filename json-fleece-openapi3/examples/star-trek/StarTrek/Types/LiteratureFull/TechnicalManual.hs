{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LiteratureFull.TechnicalManual
  ( TechnicalManual(..)
  , technicalManualSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TechnicalManual = TechnicalManual Bool
  deriving (Show, Eq)

technicalManualSchema :: FC.Fleece schema => schema TechnicalManual
technicalManualSchema =
  FC.coerceSchema FC.boolean