{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.Producer
  ( Producer(..)
  , producerSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Producer = Producer Bool
  deriving (Show, Eq)

producerSchema :: FC.Fleece schema => schema Producer
producerSchema =
  FC.coerceSchema FC.boolean