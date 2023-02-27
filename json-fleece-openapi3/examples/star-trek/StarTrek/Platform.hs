{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Platform
  ( Platform(..)
  , platformSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.Platform.Name (Name, nameSchema)
import StarTrek.Platform.Uid (Uid, uidSchema)

data Platform = Platform
  { name :: Maybe Name -- ^ Platform name
  , uid :: Maybe Uid -- ^ Platform unique ID
  }
  deriving (Eq, Show)

platformSchema :: FC.Fleece schema => schema Platform
platformSchema =
  FC.object $
    FC.constructor Platform
      #+ FC.optional "name" name nameSchema
      #+ FC.optional "uid" uid uidSchema