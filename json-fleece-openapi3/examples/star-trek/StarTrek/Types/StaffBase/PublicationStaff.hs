{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.PublicationStaff
  ( PublicationStaff(..)
  , publicationStaffSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PublicationStaff = PublicationStaff Bool
  deriving (Show, Eq)

publicationStaffSchema :: FC.Fleece t => FC.Schema t PublicationStaff
publicationStaffSchema =
  FC.coerceSchema FC.boolean