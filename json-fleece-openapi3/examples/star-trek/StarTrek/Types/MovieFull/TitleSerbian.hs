{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MovieFull.TitleSerbian
  ( TitleSerbian(..)
  , titleSerbianSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleSerbian = TitleSerbian T.Text
  deriving (Show, Eq)

titleSerbianSchema :: FC.Fleece t => FC.Schema t TitleSerbian
titleSerbianSchema =
  FC.coerceSchema FC.text