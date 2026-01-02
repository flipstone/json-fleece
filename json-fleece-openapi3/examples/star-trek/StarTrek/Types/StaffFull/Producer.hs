{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.Producer
  ( Producer(..)
  , producerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Producer = Producer Bool
  deriving (Show, Eq)

producerSchema :: FC.Fleece t => FC.Schema t Producer
producerSchema =
  FC.coerceSchema FC.boolean