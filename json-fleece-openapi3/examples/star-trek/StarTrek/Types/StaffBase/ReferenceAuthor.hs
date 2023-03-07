{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.ReferenceAuthor
  ( ReferenceAuthor(..)
  , referenceAuthorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ReferenceAuthor = ReferenceAuthor Bool
  deriving (Show, Eq)

referenceAuthorSchema :: FC.Fleece schema => schema ReferenceAuthor
referenceAuthorSchema =
  FC.coerceSchema FC.boolean