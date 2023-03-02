{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CompanyFull.TvAndFilmProductionCompany
  ( TvAndFilmProductionCompany(..)
  , tvAndFilmProductionCompanySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TvAndFilmProductionCompany = TvAndFilmProductionCompany Bool
  deriving (Show, Eq)

tvAndFilmProductionCompanySchema :: FC.Fleece schema => schema TvAndFilmProductionCompany
tvAndFilmProductionCompanySchema =
  FC.coerceSchema FC.boolean