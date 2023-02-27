{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieFull.TitleSpanish
  ( TitleSpanish(..)
  , titleSpanishSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleSpanish = TitleSpanish Text
  deriving (Show, Eq)

titleSpanishSchema :: FC.Fleece schema => schema TitleSpanish
titleSpanishSchema =
  FC.coerceSchema FC.text