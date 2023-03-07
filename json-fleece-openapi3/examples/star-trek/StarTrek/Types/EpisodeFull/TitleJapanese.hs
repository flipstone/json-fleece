{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.EpisodeFull.TitleJapanese
  ( TitleJapanese(..)
  , titleJapaneseSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleJapanese = TitleJapanese T.Text
  deriving (Show, Eq)

titleJapaneseSchema :: FC.Fleece schema => schema TitleJapanese
titleJapaneseSchema =
  FC.coerceSchema FC.text