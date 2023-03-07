{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpacecraftBase.DateStatus
  ( DateStatus(..)
  , dateStatusSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype DateStatus = DateStatus T.Text
  deriving (Show, Eq)

dateStatusSchema :: FC.Fleece schema => schema DateStatus
dateStatusSchema =
  FC.coerceSchema FC.text