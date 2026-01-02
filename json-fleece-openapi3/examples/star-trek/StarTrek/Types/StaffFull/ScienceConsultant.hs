{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.ScienceConsultant
  ( ScienceConsultant(..)
  , scienceConsultantSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ScienceConsultant = ScienceConsultant Bool
  deriving (Show, Eq)

scienceConsultantSchema :: FC.Fleece t => FC.Schema t ScienceConsultant
scienceConsultantSchema =
  FC.coerceSchema FC.boolean