{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookBase.ProductionNumber
  ( ProductionNumber(..)
  , productionNumberSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ProductionNumber = ProductionNumber T.Text
  deriving (Show, Eq)

productionNumberSchema :: FC.Fleece t => FC.Schema t ProductionNumber
productionNumberSchema =
  FC.coerceSchema FC.text