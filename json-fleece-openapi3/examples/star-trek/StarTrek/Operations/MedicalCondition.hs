{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.MedicalCondition
  ( MedicalCondition(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data MedicalCondition = MedicalCondition
  deriving (Eq, Show)

route :: R.Router r => r MedicalCondition
route =
  R.get $
    R.make MedicalCondition
      /- "medicalCondition"