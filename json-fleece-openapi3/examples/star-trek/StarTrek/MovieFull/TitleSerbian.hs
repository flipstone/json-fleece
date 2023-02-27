{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieFull.TitleSerbian
  ( TitleSerbian(..)
  , titleSerbianSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleSerbian = TitleSerbian Text
  deriving (Show, Eq)

titleSerbianSchema :: FC.Fleece schema => schema TitleSerbian
titleSerbianSchema =
  FC.coerceSchema FC.text