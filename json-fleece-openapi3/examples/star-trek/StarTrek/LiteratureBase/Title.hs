{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LiteratureBase.Title
  ( Title(..)
  , titleSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Title = Title Text
  deriving (Show, Eq)

titleSchema :: FC.Fleece schema => schema Title
titleSchema =
  FC.coerceSchema FC.text