{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieFull.TitleRussian
  ( TitleRussian(..)
  , titleRussianSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleRussian = TitleRussian T.Text
  deriving (Show, Eq)

titleRussianSchema :: FC.Fleece schema => schema TitleRussian
titleRussianSchema =
  FC.coerceSchema FC.text