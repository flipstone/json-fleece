{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TitleHeader
  ( TitleHeader(..)
  , titleHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data TitleHeader = TitleHeader
  { name :: Text -- ^ Title name
  , uid :: Text -- ^ Title unique ID
  }
  deriving (Eq, Show)

titleHeaderSchema :: FC.Fleece schema => schema TitleHeader
titleHeaderSchema =
  FC.object $
    FC.constructor TitleHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text