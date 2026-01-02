{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.Platform
  ( Platform(..)
  , platformSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.Platform.Name as Name
import qualified StarTrek.Types.Platform.Uid as Uid

data Platform = Platform
  { name :: Maybe Name.Name -- ^ Platform name
  , uid :: Maybe Uid.Uid -- ^ Platform unique ID
  }
  deriving (Eq, Show)

platformSchema :: FC.Fleece t => FC.Schema t Platform
platformSchema =
  FC.object $
    FC.constructor Platform
      #+ FC.optional "name" name Name.nameSchema
      #+ FC.optional "uid" uid Uid.uidSchema