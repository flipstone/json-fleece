{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MovieBase.TitlePolish
  ( TitlePolish(..)
  , titlePolishSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype TitlePolish = TitlePolish Text
  deriving (Show, Eq)

titlePolishSchema :: FC.Fleece schema => schema TitlePolish
titlePolishSchema =
  FC.coerceSchema FC.text