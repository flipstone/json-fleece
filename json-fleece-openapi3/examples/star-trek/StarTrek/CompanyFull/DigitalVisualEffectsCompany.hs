{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyFull.DigitalVisualEffectsCompany
  ( DigitalVisualEffectsCompany(..)
  , digitalVisualEffectsCompanySchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype DigitalVisualEffectsCompany = DigitalVisualEffectsCompany Bool
  deriving (Show, Eq)

digitalVisualEffectsCompanySchema :: FC.Fleece schema => schema DigitalVisualEffectsCompany
digitalVisualEffectsCompanySchema =
  FC.coerceSchema FC.boolean