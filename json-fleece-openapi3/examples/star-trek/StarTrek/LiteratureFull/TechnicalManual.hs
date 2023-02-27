{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LiteratureFull.TechnicalManual
  ( TechnicalManual(..)
  , technicalManualSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TechnicalManual = TechnicalManual Bool
  deriving (Show, Eq)

technicalManualSchema :: FC.Fleece schema => schema TechnicalManual
technicalManualSchema =
  FC.coerceSchema FC.boolean