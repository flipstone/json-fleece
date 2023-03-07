{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MagazineSeriesFull.NumberOfIssues
  ( NumberOfIssues(..)
  , numberOfIssuesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype NumberOfIssues = NumberOfIssues Integer
  deriving (Show, Eq)

numberOfIssuesSchema :: FC.Fleece schema => schema NumberOfIssues
numberOfIssuesSchema =
  FC.coerceSchema FC.integer