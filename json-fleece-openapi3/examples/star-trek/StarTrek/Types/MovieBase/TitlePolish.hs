{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MovieBase.TitlePolish
  ( TitlePolish(..)
  , titlePolishSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitlePolish = TitlePolish T.Text
  deriving (Show, Eq)

titlePolishSchema :: FC.Fleece schema => schema TitlePolish
titlePolishSchema =
  FC.coerceSchema FC.text