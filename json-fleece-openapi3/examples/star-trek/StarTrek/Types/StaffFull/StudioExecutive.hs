{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.StudioExecutive
  ( StudioExecutive(..)
  , studioExecutiveSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype StudioExecutive = StudioExecutive Bool
  deriving (Show, Eq)

studioExecutiveSchema :: FC.Fleece t => FC.Schema t StudioExecutive
studioExecutiveSchema =
  FC.coerceSchema FC.boolean