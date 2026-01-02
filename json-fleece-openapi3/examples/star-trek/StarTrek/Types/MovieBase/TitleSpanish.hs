{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MovieBase.TitleSpanish
  ( TitleSpanish(..)
  , titleSpanishSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleSpanish = TitleSpanish T.Text
  deriving (Show, Eq)

titleSpanishSchema :: FC.Fleece t => FC.Schema t TitleSpanish
titleSpanishSchema =
  FC.coerceSchema FC.text