{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyBase.BorgComponent
  ( BorgComponent(..)
  , borgComponentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype BorgComponent = BorgComponent Bool
  deriving (Show, Eq)

borgComponentSchema :: FC.Fleece schema => schema BorgComponent
borgComponentSchema =
  FC.coerceSchema FC.boolean