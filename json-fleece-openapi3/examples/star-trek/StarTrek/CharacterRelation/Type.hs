{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterRelation.Type
  ( Type(..)
  , typeSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Type = Type Text
  deriving (Show, Eq)

typeSchema :: FC.Fleece schema => schema Type
typeSchema =
  FC.coerceSchema FC.text