{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SoundtrackFull
  ( SoundtrackFull(..)
  , soundtrackFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.CompanyBase as CompanyBase
import qualified StarTrek.Types.Reference as Reference
import qualified StarTrek.Types.SoundtrackFull.Length as Length
import qualified StarTrek.Types.SoundtrackFull.ReleaseDate as ReleaseDate
import qualified StarTrek.Types.SoundtrackFull.Title as Title
import qualified StarTrek.Types.SoundtrackFull.Uid as Uid
import qualified StarTrek.Types.StaffBase as StaffBase

data SoundtrackFull = SoundtrackFull
  { composers :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , contributors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , labels :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , length :: Maybe Length.Length -- ^ Length, in seconds
  , orchestrators :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , references :: Maybe [Reference.Reference] -- ^ Reference of book, comics, video release, etc.
  , releaseDate :: Maybe ReleaseDate.ReleaseDate -- ^ Release date
  , title :: Title.Title -- ^ Soundtrack title
  , uid :: Uid.Uid -- ^ Soundtrack unique ID
  }
  deriving (Eq, Show)

soundtrackFullSchema :: FC.Fleece t => FC.Schema t SoundtrackFull
soundtrackFullSchema =
  FC.object $
    FC.constructor SoundtrackFull
      #+ FC.optional "composers" composers (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "contributors" contributors (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "labels" labels (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "length" length Length.lengthSchema
      #+ FC.optional "orchestrators" orchestrators (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "references" references (FC.list Reference.referenceSchema)
      #+ FC.optional "releaseDate" releaseDate ReleaseDate.releaseDateSchema
      #+ FC.required "title" title Title.titleSchema
      #+ FC.required "uid" uid Uid.uidSchema