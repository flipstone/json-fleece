{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicSeriesHeader.Title
  ( Title(..)
  , titleSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Title = Title T.Text
  deriving (Show, Eq)

titleSchema :: FC.Fleece schema => schema Title
titleSchema =
  FC.coerceSchema FC.text