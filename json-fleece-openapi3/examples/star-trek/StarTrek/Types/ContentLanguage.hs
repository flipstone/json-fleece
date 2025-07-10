{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ContentLanguage
  ( ContentLanguage(..)
  , contentLanguageSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ContentLanguage.Iso6391Code as Iso6391Code
import qualified StarTrek.Types.ContentLanguage.Name as Name
import qualified StarTrek.Types.ContentLanguage.Uid as Uid

data ContentLanguage = ContentLanguage
  { iso6391Code :: Maybe Iso6391Code.Iso6391Code -- ^ ISO 639-1 code
  , name :: Maybe Name.Name -- ^ Language name
  , uid :: Maybe Uid.Uid -- ^ Language unique ID
  }
  deriving (Eq, Show)

contentLanguageSchema :: FC.Fleece schema => schema ContentLanguage
contentLanguageSchema =
  FC.object $
    FC.constructor ContentLanguage
      #+ FC.optional "iso6391Code" iso6391Code Iso6391Code.iso6391CodeSchema
      #+ FC.optional "name" name Name.nameSchema
      #+ FC.optional "uid" uid Uid.uidSchema