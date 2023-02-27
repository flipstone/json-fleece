{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeasonHeader
  ( SeasonHeader(..)
  , seasonHeaderSchema
  ) where

import Data.Text (Text)
import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)

data SeasonHeader = SeasonHeader
  { uid :: Text -- ^ Season unique ID
  , title :: Text -- ^ Season title
  }
  deriving (Eq, Show)

seasonHeaderSchema :: FC.Fleece schema => schema SeasonHeader
seasonHeaderSchema =
  FC.object $
    FC.constructor SeasonHeader
      #+ FC.required "uid" uid FC.text
      #+ FC.required "title" title FC.text