{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyHeader
  ( CompanyHeader(..)
  , companyHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data CompanyHeader = CompanyHeader
  { name :: Text -- ^ Company title
  , uid :: Text -- ^ Company unique ID
  }
  deriving (Eq, Show)

companyHeaderSchema :: FC.Fleece schema => schema CompanyHeader
companyHeaderSchema =
  FC.object $
    FC.constructor CompanyHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text