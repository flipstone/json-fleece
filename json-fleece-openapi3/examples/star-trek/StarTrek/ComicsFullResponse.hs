{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicsFullResponse
  ( ComicsFullResponse(..)
  , comicsFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ComicsFull (ComicsFull, comicsFullSchema)

data ComicsFullResponse = ComicsFullResponse
  { comics :: Maybe ComicsFull -- ^ Full comics, returned when queried using UID
  }
  deriving (Eq, Show)

comicsFullResponseSchema :: FC.Fleece schema => schema ComicsFullResponse
comicsFullResponseSchema =
  FC.object $
    FC.constructor ComicsFullResponse
      #+ FC.optional "comics" comics comicsFullSchema