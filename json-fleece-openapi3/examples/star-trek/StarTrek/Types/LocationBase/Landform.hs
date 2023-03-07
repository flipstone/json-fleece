{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationBase.Landform
  ( Landform(..)
  , landformSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Landform = Landform Bool
  deriving (Show, Eq)

landformSchema :: FC.Fleece schema => schema Landform
landformSchema =
  FC.coerceSchema FC.boolean