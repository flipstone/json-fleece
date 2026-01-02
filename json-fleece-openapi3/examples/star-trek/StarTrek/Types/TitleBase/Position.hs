{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TitleBase.Position
  ( Position(..)
  , positionSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Position = Position Bool
  deriving (Show, Eq)

positionSchema :: FC.Fleece t => FC.Schema t Position
positionSchema =
  FC.coerceSchema FC.boolean