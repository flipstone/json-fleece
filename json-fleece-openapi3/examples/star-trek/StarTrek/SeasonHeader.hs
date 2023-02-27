{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeasonHeader
  ( SeasonHeader(..)
  , seasonHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.SeasonHeader.Title (Title, titleSchema)
import StarTrek.SeasonHeader.Uid (Uid, uidSchema)

data SeasonHeader = SeasonHeader
  { uid :: Uid -- ^ Season unique ID
  , title :: Title -- ^ Season title
  }
  deriving (Eq, Show)

seasonHeaderSchema :: FC.Fleece schema => schema SeasonHeader
seasonHeaderSchema =
  FC.object $
    FC.constructor SeasonHeader
      #+ FC.required "uid" uid uidSchema
      #+ FC.required "title" title titleSchema