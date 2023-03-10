{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftClassBase.WarpCapable
  ( WarpCapable(..)
  , warpCapableSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype WarpCapable = WarpCapable Bool
  deriving (Show, Eq)

warpCapableSchema :: FC.Fleece schema => schema WarpCapable
warpCapableSchema =
  FC.coerceSchema FC.boolean