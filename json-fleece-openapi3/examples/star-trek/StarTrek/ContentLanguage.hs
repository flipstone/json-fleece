{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ContentLanguage
  ( ContentLanguage(..)
  , contentLanguageSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)

data ContentLanguage = ContentLanguage
  { name :: Maybe Text -- ^ Language name
  , iso6391Code :: Maybe Text -- ^ ISO 639-1 code
  , uid :: Maybe Text -- ^ Language unique ID
  }
  deriving (Eq, Show)

contentLanguageSchema :: FC.Fleece schema => schema ContentLanguage
contentLanguageSchema =
  FC.object $
    FC.constructor ContentLanguage
      #+ FC.optionalField FC.OmitKey_DelegateNull "name" name FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "iso6391Code" iso6391Code FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "uid" uid FC.text