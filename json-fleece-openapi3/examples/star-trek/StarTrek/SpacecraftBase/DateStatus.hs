{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpacecraftBase.DateStatus
  ( DateStatus(..)
  , dateStatusSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype DateStatus = DateStatus Text
  deriving (Show, Eq)

dateStatusSchema :: FC.Fleece schema => schema DateStatus
dateStatusSchema =
  FC.coerceSchema FC.text