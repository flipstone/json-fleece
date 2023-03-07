{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TitleBase.Uid
  ( Uid(..)
  , uidSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Uid = Uid T.Text
  deriving (Show, Eq)

uidSchema :: FC.Fleece schema => schema Uid
uidSchema =
  FC.coerceSchema FC.text