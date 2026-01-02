{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.Writer
  ( Writer(..)
  , writerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Writer = Writer Bool
  deriving (Show, Eq)

writerSchema :: FC.Fleece t => FC.Schema t Writer
writerSchema =
  FC.coerceSchema FC.boolean