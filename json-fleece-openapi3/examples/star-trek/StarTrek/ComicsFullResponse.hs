{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicsFullResponse
  ( ComicsFullResponse(..)
  , comicsFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.ComicsFull as ComicsFull

data ComicsFullResponse = ComicsFullResponse
  { comics :: Maybe ComicsFull.ComicsFull -- ^ Full comics, returned when queried using UID
  }
  deriving (Eq, Show)

comicsFullResponseSchema :: FC.Fleece schema => schema ComicsFullResponse
comicsFullResponseSchema =
  FC.object $
    FC.constructor ComicsFullResponse
      #+ FC.optional "comics" comics ComicsFull.comicsFullSchema