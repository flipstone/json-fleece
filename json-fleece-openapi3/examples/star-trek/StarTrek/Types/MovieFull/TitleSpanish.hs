{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MovieFull.TitleSpanish
  ( TitleSpanish(..)
  , titleSpanishSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleSpanish = TitleSpanish T.Text
  deriving (Show, Eq)

titleSpanishSchema :: FC.Fleece schema => schema TitleSpanish
titleSpanishSchema =
  FC.coerceSchema FC.text