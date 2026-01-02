{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TitleHeader
  ( TitleHeader(..)
  , titleHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.TitleHeader.Name as Name
import qualified StarTrek.Types.TitleHeader.Uid as Uid

data TitleHeader = TitleHeader
  { name :: Name.Name -- ^ Title name
  , uid :: Uid.Uid -- ^ Title unique ID
  }
  deriving (Eq, Show)

titleHeaderSchema :: FC.Fleece t => FC.Schema t TitleHeader
titleHeaderSchema =
  FC.object $
    FC.constructor TitleHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema