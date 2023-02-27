{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.ScienceConsultant
  ( ScienceConsultant(..)
  , scienceConsultantSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ScienceConsultant = ScienceConsultant Bool
  deriving (Show, Eq)

scienceConsultantSchema :: FC.Fleece schema => schema ScienceConsultant
scienceConsultantSchema =
  FC.coerceSchema FC.boolean