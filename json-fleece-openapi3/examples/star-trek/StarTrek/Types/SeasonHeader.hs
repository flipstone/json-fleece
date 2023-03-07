{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SeasonHeader
  ( SeasonHeader(..)
  , seasonHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.SeasonHeader.Title as Title
import qualified StarTrek.Types.SeasonHeader.Uid as Uid

data SeasonHeader = SeasonHeader
  { uid :: Uid.Uid -- ^ Season unique ID
  , title :: Title.Title -- ^ Season title
  }
  deriving (Eq, Show)

seasonHeaderSchema :: FC.Fleece schema => schema SeasonHeader
seasonHeaderSchema =
  FC.object $
    FC.constructor SeasonHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "title" title Title.titleSchema