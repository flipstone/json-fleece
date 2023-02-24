{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineFull
  ( MagazineFull(..)
  , magazineFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Integer, Maybe, Show)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.MagazineSeriesBase (MagazineSeriesBase, magazineSeriesBaseSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data MagazineFull = MagazineFull
  { magazineSeries :: Maybe [MagazineSeriesBase] -- ^ Magazine series this magazine is included in
  , publishers :: Maybe [CompanyBase] -- ^ Magazine publishers
  , publishedMonth :: Maybe Integer -- ^ Month the magazine was published
  , publishedYear :: Maybe Integer -- ^ Year the magazine was published
  , uid :: Text -- ^ Magazine unique ID
  , publishedDay :: Maybe Integer -- ^ Day the magazine was published
  , coverYear :: Maybe Integer -- ^ Cover publication year
  , issueNumber :: Maybe Text -- ^ Magazine issue number
  , title :: Text -- ^ Magazine title
  , coverDay :: Maybe Integer -- ^ Cover publication day
  , numberOfPages :: Maybe Integer -- ^ Number of pages
  , coverMonth :: Maybe Integer -- ^ Cover publication month
  , editors :: Maybe [StaffBase] -- ^ Editors involved in the magazine
  }
  deriving (Eq, Show)

magazineFullSchema :: FC.Fleece schema => schema MagazineFull
magazineFullSchema =
  FC.object $
    FC.constructor MagazineFull
      #+ FC.optionalField FC.OmitKey_DelegateNull "magazineSeries" magazineSeries (FC.list magazineSeriesBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishers" publishers (FC.list companyBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedMonth" publishedMonth FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedYear" publishedYear FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "publishedDay" publishedDay FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "coverYear" coverYear FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "issueNumber" issueNumber FC.text
      #+ FC.required "title" title FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "coverDay" coverDay FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "numberOfPages" numberOfPages FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "coverMonth" coverMonth FC.integer
      #+ FC.optionalField FC.OmitKey_DelegateNull "editors" editors (FC.list staffBaseSchema)