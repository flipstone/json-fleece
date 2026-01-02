{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationBase.Road
  ( Road(..)
  , roadSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Road = Road Bool
  deriving (Show, Eq)

roadSchema :: FC.Fleece t => FC.Schema t Road
roadSchema =
  FC.coerceSchema FC.boolean