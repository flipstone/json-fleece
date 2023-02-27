{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieFull.TitleCatalan
  ( TitleCatalan(..)
  , titleCatalanSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleCatalan = TitleCatalan Text
  deriving (Show, Eq)

titleCatalanSchema :: FC.Fleece schema => schema TitleCatalan
titleCatalanSchema =
  FC.coerceSchema FC.text