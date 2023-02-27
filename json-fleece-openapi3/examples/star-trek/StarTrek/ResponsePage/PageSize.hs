{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ResponsePage.PageSize
  ( PageSize(..)
  , pageSizeSchema
  ) where

import Data.Int (Int32)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype PageSize = PageSize Int32
  deriving (Show, Eq)

pageSizeSchema :: FC.Fleece schema => schema PageSize
pageSizeSchema =
  FC.coerceSchema FC.int32