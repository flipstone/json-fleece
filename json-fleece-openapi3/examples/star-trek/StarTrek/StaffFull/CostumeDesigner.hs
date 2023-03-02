{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.CostumeDesigner
  ( CostumeDesigner(..)
  , costumeDesignerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype CostumeDesigner = CostumeDesigner Bool
  deriving (Show, Eq)

costumeDesignerSchema :: FC.Fleece schema => schema CostumeDesigner
costumeDesignerSchema =
  FC.coerceSchema FC.boolean