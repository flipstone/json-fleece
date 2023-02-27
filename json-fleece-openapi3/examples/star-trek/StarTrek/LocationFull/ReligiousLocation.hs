{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationFull.ReligiousLocation
  ( ReligiousLocation(..)
  , religiousLocationSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ReligiousLocation = ReligiousLocation Bool
  deriving (Show, Eq)

religiousLocationSchema :: FC.Fleece schema => schema ReligiousLocation
religiousLocationSchema =
  FC.coerceSchema FC.boolean