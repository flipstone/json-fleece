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
  { title :: Title.Title -- ^ Soundtrack title
  , length :: Maybe Length.Length -- ^ Length, in seconds
  , composers :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , uid :: Uid.Uid -- ^ Soundtrack unique ID
  , references :: Maybe [Reference.Reference] -- ^ Reference of book, comics, video release, etc.
  , labels :: Maybe [CompanyBase.CompanyBase] -- ^ Base company, returned in search results
  , contributors :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , orchestrators :: Maybe [StaffBase.StaffBase] -- ^ Base staff, returned in search results
  , releaseDate :: Maybe ReleaseDate.ReleaseDate -- ^ Release date
  }
  deriving (Eq, Show)

soundtrackFullSchema :: FC.Fleece schema => schema SoundtrackFull
soundtrackFullSchema =
  FC.object $
    FC.constructor SoundtrackFull
      #+ FC.required "title" title Title.titleSchema
      #+ FC.optional "length" length Length.lengthSchema
      #+ FC.optional "composers" composers (FC.list StaffBase.staffBaseSchema)
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "references" references (FC.list Reference.referenceSchema)
      #+ FC.optional "labels" labels (FC.list CompanyBase.companyBaseSchema)
      #+ FC.optional "contributors" contributors (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "orchestrators" orchestrators (FC.list StaffBase.staffBaseSchema)
      #+ FC.optional "releaseDate" releaseDate ReleaseDate.releaseDateSchema