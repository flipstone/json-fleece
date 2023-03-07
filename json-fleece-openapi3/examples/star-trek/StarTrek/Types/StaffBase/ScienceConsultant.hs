{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.ScienceConsultant
  ( ScienceConsultant(..)
  , scienceConsultantSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ScienceConsultant = ScienceConsultant Bool
  deriving (Show, Eq)

scienceConsultantSchema :: FC.Fleece schema => schema ScienceConsultant
scienceConsultantSchema =
  FC.coerceSchema FC.boolean