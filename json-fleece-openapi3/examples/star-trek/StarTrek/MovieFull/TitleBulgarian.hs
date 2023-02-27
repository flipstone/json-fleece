{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieFull.TitleBulgarian
  ( TitleBulgarian(..)
  , titleBulgarianSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitleBulgarian = TitleBulgarian Text
  deriving (Show, Eq)

titleBulgarianSchema :: FC.Fleece schema => schema TitleBulgarian
titleBulgarianSchema =
  FC.coerceSchema FC.text