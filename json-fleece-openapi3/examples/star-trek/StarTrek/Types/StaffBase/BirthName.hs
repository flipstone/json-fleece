{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.BirthName
  ( BirthName(..)
  , birthNameSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype BirthName = BirthName T.Text
  deriving (Show, Eq)

birthNameSchema :: FC.Fleece schema => schema BirthName
birthNameSchema =
  FC.coerceSchema FC.text