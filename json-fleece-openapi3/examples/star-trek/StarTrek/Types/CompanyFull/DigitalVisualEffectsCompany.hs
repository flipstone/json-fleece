{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CompanyFull.DigitalVisualEffectsCompany
  ( DigitalVisualEffectsCompany(..)
  , digitalVisualEffectsCompanySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype DigitalVisualEffectsCompany = DigitalVisualEffectsCompany Bool
  deriving (Show, Eq)

digitalVisualEffectsCompanySchema :: FC.Fleece t => FC.Schema t DigitalVisualEffectsCompany
digitalVisualEffectsCompanySchema =
  FC.coerceSchema FC.boolean