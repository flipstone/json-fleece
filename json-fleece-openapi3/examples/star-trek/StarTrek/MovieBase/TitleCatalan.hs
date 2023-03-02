{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieBase.TitleCatalan
  ( TitleCatalan(..)
  , titleCatalanSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleCatalan = TitleCatalan T.Text
  deriving (Show, Eq)

titleCatalanSchema :: FC.Fleece schema => schema TitleCatalan
titleCatalanSchema =
  FC.coerceSchema FC.text