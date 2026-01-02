{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MovieFull.TitleCatalan
  ( TitleCatalan(..)
  , titleCatalanSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleCatalan = TitleCatalan T.Text
  deriving (Show, Eq)

titleCatalanSchema :: FC.Fleece t => FC.Schema t TitleCatalan
titleCatalanSchema =
  FC.coerceSchema FC.text