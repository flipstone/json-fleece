{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.PublicationStaff
  ( PublicationStaff(..)
  , publicationStaffSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PublicationStaff = PublicationStaff Bool
  deriving (Show, Eq)

publicationStaffSchema :: FC.Fleece schema => schema PublicationStaff
publicationStaffSchema =
  FC.coerceSchema FC.boolean