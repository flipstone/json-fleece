{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LiteratureFull.EarthlyOrigin
  ( EarthlyOrigin(..)
  , earthlyOriginSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype EarthlyOrigin = EarthlyOrigin Bool
  deriving (Show, Eq)

earthlyOriginSchema :: FC.Fleece schema => schema EarthlyOrigin
earthlyOriginSchema =
  FC.coerceSchema FC.boolean