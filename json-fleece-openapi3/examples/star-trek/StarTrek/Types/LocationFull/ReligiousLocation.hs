{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationFull.ReligiousLocation
  ( ReligiousLocation(..)
  , religiousLocationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ReligiousLocation = ReligiousLocation Bool
  deriving (Show, Eq)

religiousLocationSchema :: FC.Fleece t => FC.Schema t ReligiousLocation
religiousLocationSchema =
  FC.coerceSchema FC.boolean