{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftClassFull.WarpCapable
  ( WarpCapable(..)
  , warpCapableSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype WarpCapable = WarpCapable Bool
  deriving (Show, Eq)

warpCapableSchema :: FC.Fleece t => FC.Schema t WarpCapable
warpCapableSchema =
  FC.coerceSchema FC.boolean