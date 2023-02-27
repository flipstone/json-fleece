{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieBase.TitleRussian
  ( TitleRussian(..)
  , titleRussianSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleRussian = TitleRussian Text
  deriving (Show, Eq)

titleRussianSchema :: FC.Fleece schema => schema TitleRussian
titleRussianSchema =
  FC.coerceSchema FC.text