{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MovieBase.TitleItalian
  ( TitleItalian(..)
  , titleItalianSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleItalian = TitleItalian T.Text
  deriving (Show, Eq)

titleItalianSchema :: FC.Fleece t => FC.Schema t TitleItalian
titleItalianSchema =
  FC.coerceSchema FC.text