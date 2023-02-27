{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Reference.ReferenceNumber
  ( ReferenceNumber(..)
  , referenceNumberSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ReferenceNumber = ReferenceNumber Text
  deriving (Show, Eq)

referenceNumberSchema :: FC.Fleece schema => schema ReferenceNumber
referenceNumberSchema =
  FC.coerceSchema FC.text