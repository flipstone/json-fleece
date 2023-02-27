{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ResponsePage.PageNumber
  ( PageNumber(..)
  , pageNumberSchema
  ) where

import Data.Int (Int32)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype PageNumber = PageNumber Int32
  deriving (Show, Eq)

pageNumberSchema :: FC.Fleece schema => schema PageNumber
pageNumberSchema =
  FC.coerceSchema FC.int32