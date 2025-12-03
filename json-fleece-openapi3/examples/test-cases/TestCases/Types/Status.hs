{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.Status
  ( Status(..)
  , statusSchema
  , statusToText
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

data Status
  = GOOD
  | BAD
  | UGLY
  deriving (Eq, Show, Ord, Enum, Bounded)

statusToText :: Status -> T.Text
statusToText v =
  T.pack $
    case v of
      GOOD -> "GOOD"
      BAD -> "BAD"
      UGLY -> "UGLY"

statusSchema :: FC.Fleece schema => schema Status
statusSchema =
  FC.boundedEnum statusToText