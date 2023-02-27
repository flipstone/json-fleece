{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Platform
  ( Platform(..)
  , platformSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)

data Platform = Platform
  { name :: Maybe Text -- ^ Platform name
  , uid :: Maybe Text -- ^ Platform unique ID
  }
  deriving (Eq, Show)

platformSchema :: FC.Fleece schema => schema Platform
platformSchema =
  FC.object $
    FC.constructor Platform
      #+ FC.optional "name" name FC.text
      #+ FC.optional "uid" uid FC.text