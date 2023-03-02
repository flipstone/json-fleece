{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TitleHeader
  ( TitleHeader(..)
  , titleHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.TitleHeader.Name as Name
import qualified StarTrek.TitleHeader.Uid as Uid

data TitleHeader = TitleHeader
  { name :: Name.Name -- ^ Title name
  , uid :: Uid.Uid -- ^ Title unique ID
  }
  deriving (Eq, Show)

titleHeaderSchema :: FC.Fleece schema => schema TitleHeader
titleHeaderSchema =
  FC.object $
    FC.constructor TitleHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema