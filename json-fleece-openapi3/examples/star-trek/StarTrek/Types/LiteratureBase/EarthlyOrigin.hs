{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LiteratureBase.EarthlyOrigin
  ( EarthlyOrigin(..)
  , earthlyOriginSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype EarthlyOrigin = EarthlyOrigin Bool
  deriving (Show, Eq)

earthlyOriginSchema :: FC.Fleece t => FC.Schema t EarthlyOrigin
earthlyOriginSchema =
  FC.coerceSchema FC.boolean