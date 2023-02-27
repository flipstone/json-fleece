{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicStripBase.Periodical
  ( Periodical(..)
  , periodicalSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Periodical = Periodical Text
  deriving (Show, Eq)

periodicalSchema :: FC.Fleece schema => schema Periodical
periodicalSchema =
  FC.coerceSchema FC.text