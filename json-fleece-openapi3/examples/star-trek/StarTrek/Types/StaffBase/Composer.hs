{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.Composer
  ( Composer(..)
  , composerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Composer = Composer Bool
  deriving (Show, Eq)

composerSchema :: FC.Fleece t => FC.Schema t Composer
composerSchema =
  FC.coerceSchema FC.boolean