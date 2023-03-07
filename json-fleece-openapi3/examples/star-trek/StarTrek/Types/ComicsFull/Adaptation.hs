{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicsFull.Adaptation
  ( Adaptation(..)
  , adaptationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Adaptation = Adaptation Bool
  deriving (Show, Eq)

adaptationSchema :: FC.Fleece schema => schema Adaptation
adaptationSchema =
  FC.coerceSchema FC.boolean