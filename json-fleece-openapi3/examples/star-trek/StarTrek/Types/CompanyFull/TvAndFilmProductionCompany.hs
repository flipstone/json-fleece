{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CompanyFull.TvAndFilmProductionCompany
  ( TvAndFilmProductionCompany(..)
  , tvAndFilmProductionCompanySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TvAndFilmProductionCompany = TvAndFilmProductionCompany Bool
  deriving (Show, Eq)

tvAndFilmProductionCompanySchema :: FC.Fleece t => FC.Schema t TvAndFilmProductionCompany
tvAndFilmProductionCompanySchema =
  FC.coerceSchema FC.boolean