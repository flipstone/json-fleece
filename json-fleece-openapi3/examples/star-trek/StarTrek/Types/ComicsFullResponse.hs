{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicsFullResponse
  ( ComicsFullResponse(..)
  , comicsFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ComicsFull as ComicsFull

data ComicsFullResponse = ComicsFullResponse
  { comics :: Maybe ComicsFull.ComicsFull -- ^ Full comics, returned when queried using UID
  }
  deriving (Eq, Show)

comicsFullResponseSchema :: FC.Fleece t => FC.Schema t ComicsFullResponse
comicsFullResponseSchema =
  FC.object $
    FC.constructor ComicsFullResponse
      #+ FC.optional "comics" comics ComicsFull.comicsFullSchema