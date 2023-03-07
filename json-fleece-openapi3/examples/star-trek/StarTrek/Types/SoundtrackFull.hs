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
  { releaseDate :: Maybe ReleaseDate.ReleaseDate -- ^ Release date
  , uid :: Uid.Uid -- ^ Soundtrack unique ID
  , length :: Maybe Length.Length -- ^ Length, in seconds
  , composers :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , title :: Title.Title -- ^ Soundtrack title
  , labels :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , references :: Maybe [Reference.Reference] -- ^ Reference of book, comics, video release, etc.
  , orchestrators :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , contributors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  }
  deriving (Eq, Show)

soundtrackFullSchema :: FC.Fleece schema => schema SoundtrackFull
soundtrackFullSchema =
  FC.object $
    FC.constructor SoundtrackFull
      #+ FC.optional "releaseDate" releaseDate ReleaseDate.releaseDateSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "length" length Length.lengthSchema
      #+ FC.optional "composers" composers (FC.list StaffBase.staffBaseSchema)
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "labels" labels (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "references" references (FC.list Reference.referenceSchema)
      #+ FC.optional "orchestrators" orchestrators (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "contributors" contributors (FC.list StaffBase.staffBaseSchema)