{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookFull.ProductionNumber
  ( ProductionNumber(..)
  , productionNumberSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ProductionNumber = ProductionNumber Text
  deriving (Show, Eq)

productionNumberSchema :: FC.Fleece schema => schema ProductionNumber
productionNumberSchema =
  FC.coerceSchema FC.text