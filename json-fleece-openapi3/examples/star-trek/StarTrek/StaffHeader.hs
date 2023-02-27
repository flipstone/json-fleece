{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffHeader
  ( StaffHeader(..)
  , staffHeaderSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)

data StaffHeader = StaffHeader
  { name :: Text -- ^ Staff name
  , uid :: Text -- ^ Staff unique ID
  }
  deriving (Eq, Show)

staffHeaderSchema :: FC.Fleece schema => schema StaffHeader
staffHeaderSchema =
  FC.object $
    FC.constructor StaffHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text