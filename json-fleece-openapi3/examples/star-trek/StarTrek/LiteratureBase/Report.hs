{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LiteratureBase.Report
  ( Report(..)
  , reportSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Report = Report Bool
  deriving (Show, Eq)

reportSchema :: FC.Fleece schema => schema Report
reportSchema =
  FC.coerceSchema FC.boolean