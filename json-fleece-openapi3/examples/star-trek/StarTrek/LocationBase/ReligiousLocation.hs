{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationBase.ReligiousLocation
  ( ReligiousLocation(..)
  , religiousLocationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ReligiousLocation = ReligiousLocation Bool
  deriving (Show, Eq)

religiousLocationSchema :: FC.Fleece schema => schema ReligiousLocation
religiousLocationSchema =
  FC.coerceSchema FC.boolean