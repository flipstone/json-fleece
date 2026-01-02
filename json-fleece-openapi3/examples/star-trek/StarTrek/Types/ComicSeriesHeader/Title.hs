{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicSeriesHeader.Title
  ( Title(..)
  , titleSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Title = Title T.Text
  deriving (Show, Eq)

titleSchema :: FC.Fleece t => FC.Schema t Title
titleSchema =
  FC.coerceSchema FC.text