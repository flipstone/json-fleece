{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieFull.TitleJapanese
  ( TitleJapanese(..)
  , titleJapaneseSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleJapanese = TitleJapanese Text
  deriving (Show, Eq)

titleJapaneseSchema :: FC.Fleece schema => schema TitleJapanese
titleJapaneseSchema =
  FC.coerceSchema FC.text