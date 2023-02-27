{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineBase.IssueNumber
  ( IssueNumber(..)
  , issueNumberSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype IssueNumber = IssueNumber Text
  deriving (Show, Eq)

issueNumberSchema :: FC.Fleece schema => schema IssueNumber
issueNumberSchema =
  FC.coerceSchema FC.text