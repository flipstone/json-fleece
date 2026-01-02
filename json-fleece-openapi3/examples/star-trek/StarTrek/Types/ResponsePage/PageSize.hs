{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ResponsePage.PageSize
  ( PageSize(..)
  , pageSizeSchema
  ) where

import qualified Data.Int as I
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype PageSize = PageSize I.Int32
  deriving (Show, Eq)

pageSizeSchema :: FC.Fleece t => FC.Schema t PageSize
pageSizeSchema =
  FC.coerceSchema FC.int32