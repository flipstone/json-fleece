{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MovieFull.TitleBulgarian
  ( TitleBulgarian(..)
  , titleBulgarianSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleBulgarian = TitleBulgarian T.Text
  deriving (Show, Eq)

titleBulgarianSchema :: FC.Fleece schema => schema TitleBulgarian
titleBulgarianSchema =
  FC.coerceSchema FC.text