{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookBase.ProductionNumber
  ( ProductionNumber(..)
  , productionNumberSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ProductionNumber = ProductionNumber T.Text
  deriving (Show, Eq)

productionNumberSchema :: FC.Fleece schema => schema ProductionNumber
productionNumberSchema =
  FC.coerceSchema FC.text