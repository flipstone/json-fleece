{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.DerivingNothing
  ( DerivingNothing(..)
  , derivingNothingSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC

newtype DerivingNothing = DerivingNothing T.Text
  deriving ()

derivingNothingSchema :: FC.Fleece schema => schema DerivingNothing
derivingNothingSchema =
  FC.coerceSchema FC.text