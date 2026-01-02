{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MovieBase.TitleBulgarian
  ( TitleBulgarian(..)
  , titleBulgarianSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleBulgarian = TitleBulgarian T.Text
  deriving (Show, Eq)

titleBulgarianSchema :: FC.Fleece t => FC.Schema t TitleBulgarian
titleBulgarianSchema =
  FC.coerceSchema FC.text