{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TitleHeader
  ( TitleHeader(..)
  , titleHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.TitleHeader.Name (Name, nameSchema)
import StarTrek.TitleHeader.Uid (Uid, uidSchema)

data TitleHeader = TitleHeader
  { name :: Name -- ^ Title name
  , uid :: Uid -- ^ Title unique ID
  }
  deriving (Eq, Show)

titleHeaderSchema :: FC.Fleece schema => schema TitleHeader
titleHeaderSchema =
  FC.object $
    FC.constructor TitleHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema