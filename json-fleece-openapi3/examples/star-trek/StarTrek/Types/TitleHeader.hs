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
  { uid :: Uid.Uid -- ^ Title unique ID
  , name :: Name.Name -- ^ Title name
  }
  deriving (Eq, Show)

titleHeaderSchema :: FC.Fleece schema => schema TitleHeader
titleHeaderSchema =
  FC.object $
    FC.constructor TitleHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema