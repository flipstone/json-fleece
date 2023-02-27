{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieBase.TitleItalian
  ( TitleItalian(..)
  , titleItalianSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleItalian = TitleItalian Text
  deriving (Show, Eq)

titleItalianSchema :: FC.Fleece schema => schema TitleItalian
titleItalianSchema =
  FC.coerceSchema FC.text