{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerBase.BirthName
  ( BirthName(..)
  , birthNameSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype BirthName = BirthName Text
  deriving (Show, Eq)

birthNameSchema :: FC.Fleece schema => schema BirthName
birthNameSchema =
  FC.coerceSchema FC.text