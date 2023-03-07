{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MagazineBase.IssueNumber
  ( IssueNumber(..)
  , issueNumberSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype IssueNumber = IssueNumber T.Text
  deriving (Show, Eq)

issueNumberSchema :: FC.Fleece schema => schema IssueNumber
issueNumberSchema =
  FC.coerceSchema FC.text