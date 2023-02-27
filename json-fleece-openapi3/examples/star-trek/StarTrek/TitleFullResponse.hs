{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TitleFullResponse
  ( TitleFullResponse(..)
  , titleFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.TitleFull (TitleFull, titleFullSchema)

data TitleFullResponse = TitleFullResponse
  { title :: Maybe TitleFull -- ^ Full title, returned when queried using UID
  }
  deriving (Eq, Show)

titleFullResponseSchema :: FC.Fleece schema => schema TitleFullResponse
titleFullResponseSchema =
  FC.object $
    FC.constructor TitleFullResponse
      #+ FC.optional "title" title titleFullSchema