{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Platform
  ( Platform(..)
  , platformSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Platform.Name as Name
import qualified StarTrek.Platform.Uid as Uid

data Platform = Platform
  { name :: Maybe Name.Name -- ^ Platform name
  , uid :: Maybe Uid.Uid -- ^ Platform unique ID
  }
  deriving (Eq, Show)

platformSchema :: FC.Fleece schema => schema Platform
platformSchema =
  FC.object $
    FC.constructor Platform
      #+ FC.optional "name" name Name.nameSchema
      #+ FC.optional "uid" uid Uid.uidSchema