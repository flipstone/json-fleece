{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Reference.ReferenceNumber
  ( ReferenceNumber(..)
  , referenceNumberSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ReferenceNumber = ReferenceNumber T.Text
  deriving (Show, Eq)

referenceNumberSchema :: FC.Fleece schema => schema ReferenceNumber
referenceNumberSchema =
  FC.coerceSchema FC.text