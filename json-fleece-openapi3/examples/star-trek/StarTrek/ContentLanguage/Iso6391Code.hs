{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ContentLanguage.Iso6391Code
  ( Iso6391Code(..)
  , iso6391CodeSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Iso6391Code = Iso6391Code Text
  deriving (Show, Eq)

iso6391CodeSchema :: FC.Fleece schema => schema Iso6391Code
iso6391CodeSchema =
  FC.coerceSchema FC.text