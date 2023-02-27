{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.Writer
  ( Writer(..)
  , writerSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Writer = Writer Bool
  deriving (Show, Eq)

writerSchema :: FC.Fleece schema => schema Writer
writerSchema =
  FC.coerceSchema FC.boolean