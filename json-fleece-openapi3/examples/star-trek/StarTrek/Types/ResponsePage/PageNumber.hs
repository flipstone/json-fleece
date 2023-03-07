{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ResponsePage.PageNumber
  ( PageNumber(..)
  , pageNumberSchema
  ) where

import qualified Data.Int as I
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype PageNumber = PageNumber I.Int32
  deriving (Show, Eq)

pageNumberSchema :: FC.Fleece schema => schema PageNumber
pageNumberSchema =
  FC.coerceSchema FC.int32