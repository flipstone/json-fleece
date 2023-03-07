{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TitleFull.Position
  ( Position(..)
  , positionSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Position = Position Bool
  deriving (Show, Eq)

positionSchema :: FC.Fleece schema => schema Position
positionSchema =
  FC.coerceSchema FC.boolean