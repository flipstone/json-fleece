{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TitleFullResponse
  ( TitleFullResponse(..)
  , titleFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.TitleFull as TitleFull

data TitleFullResponse = TitleFullResponse
  { title :: Maybe TitleFull.TitleFull -- ^ Full title, returned when queried using UID
  }
  deriving (Eq, Show)

titleFullResponseSchema :: FC.Fleece schema => schema TitleFullResponse
titleFullResponseSchema =
  FC.object $
    FC.constructor TitleFullResponse
      #+ FC.optional "title" title TitleFull.titleFullSchema