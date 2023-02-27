{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ContentLanguage
  ( ContentLanguage(..)
  , contentLanguageSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ContentLanguage.Iso6391Code (Iso6391Code, iso6391CodeSchema)
import StarTrek.ContentLanguage.Name (Name, nameSchema)
import StarTrek.ContentLanguage.Uid (Uid, uidSchema)

data ContentLanguage = ContentLanguage
  { name :: Maybe Name -- ^ Language name
  , iso6391Code :: Maybe Iso6391Code -- ^ ISO 639-1 code
  , uid :: Maybe Uid -- ^ Language unique ID
  }
  deriving (Eq, Show)

contentLanguageSchema :: FC.Fleece schema => schema ContentLanguage
contentLanguageSchema =
  FC.object $
    FC.constructor ContentLanguage
      #+ FC.optional "name" name nameSchema
      #+ FC.optional "iso6391Code" iso6391Code iso6391CodeSchema
      #+ FC.optional "uid" uid uidSchema