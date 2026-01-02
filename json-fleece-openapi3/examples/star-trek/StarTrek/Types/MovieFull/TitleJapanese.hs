{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MovieFull.TitleJapanese
  ( TitleJapanese(..)
  , titleJapaneseSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleJapanese = TitleJapanese T.Text
  deriving (Show, Eq)

titleJapaneseSchema :: FC.Fleece t => FC.Schema t TitleJapanese
titleJapaneseSchema =
  FC.coerceSchema FC.text