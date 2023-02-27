{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationBase.Road
  ( Road(..)
  , roadSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Road = Road Bool
  deriving (Show, Eq)

roadSchema :: FC.Fleece schema => schema Road
roadSchema =
  FC.coerceSchema FC.boolean