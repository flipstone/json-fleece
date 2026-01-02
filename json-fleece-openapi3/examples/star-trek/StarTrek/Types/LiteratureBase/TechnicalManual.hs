{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LiteratureBase.TechnicalManual
  ( TechnicalManual(..)
  , technicalManualSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TechnicalManual = TechnicalManual Bool
  deriving (Show, Eq)

technicalManualSchema :: FC.Fleece t => FC.Schema t TechnicalManual
technicalManualSchema =
  FC.coerceSchema FC.boolean