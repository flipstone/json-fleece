{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SoundtrackFull
  ( SoundtrackFull(..)
  , soundtrackFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.CompanyBase (CompanyBase, companyBaseSchema)
import StarTrek.Reference (Reference, referenceSchema)
import StarTrek.SoundtrackFull.Length (Length, lengthSchema)
import StarTrek.SoundtrackFull.ReleaseDate (ReleaseDate, releaseDateSchema)
import StarTrek.SoundtrackFull.Title (Title, titleSchema)
import StarTrek.SoundtrackFull.Uid (Uid, uidSchema)
import StarTrek.StaffBase (StaffBase, staffBaseSchema)

data SoundtrackFull = SoundtrackFull
  { releaseDate :: Maybe ReleaseDate -- ^ Release date
  , uid :: Uid -- ^ Soundtrack unique ID
  , length :: Maybe Length -- ^ Length, in seconds
  , composers :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , title :: Title -- ^ Soundtrack title
  , labels :: Maybe [CompanyBase] -- ^ Base company, returned in search results
  , references :: Maybe [Reference] -- ^ Reference of book, comics, video release, etc.
  , orchestrators :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  , contributors :: Maybe [StaffBase] -- ^ Base staff, returned in search results
  }
  deriving (Eq, Show)

soundtrackFullSchema :: FC.Fleece schema => schema SoundtrackFull
soundtrackFullSchema =
  FC.object $
    FC.constructor SoundtrackFull
      #+ FC.optional "releaseDate" releaseDate releaseDateSchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "length" length lengthSchema
      #+ FC.optional "composers" composers (FC.list staffBaseSchema)
      #+ FC.required "title" title titleSchema
      #+ FC.optional "labels" labels (FC.list companyBaseSchema)
      #+ FC.optional "references" references (FC.list referenceSchema)
      #+ FC.optional "orchestrators" orchestrators (FC.list staffBaseSchema)
      #+ FC.optional "contributors" contributors (FC.list staffBaseSchema)