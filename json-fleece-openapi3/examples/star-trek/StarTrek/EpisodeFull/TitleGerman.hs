{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.EpisodeFull.TitleGerman
  ( TitleGerman(..)
  , titleGermanSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleGerman = TitleGerman T.Text
  deriving (Show, Eq)

titleGermanSchema :: FC.Fleece schema => schema TitleGerman
titleGermanSchema =
  FC.coerceSchema FC.text