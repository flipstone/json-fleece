{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CompanyFullResponse
  ( CompanyFullResponse(..)
  , companyFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CompanyFull as CompanyFull

data CompanyFullResponse = CompanyFullResponse
  { company :: Maybe CompanyFull.CompanyFull -- ^ Full company, returned when queried using UID
  }
  deriving (Eq, Show)

companyFullResponseSchema :: FC.Fleece t => FC.Schema t CompanyFullResponse
companyFullResponseSchema =
  FC.object $
    FC.constructor CompanyFullResponse
      #+ FC.optional "company" company CompanyFull.companyFullSchema