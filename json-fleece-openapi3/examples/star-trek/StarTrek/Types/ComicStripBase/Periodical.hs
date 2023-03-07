{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicStripBase.Periodical
  ( Periodical(..)
  , periodicalSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Periodical = Periodical T.Text
  deriving (Show, Eq)

periodicalSchema :: FC.Fleece schema => schema Periodical
periodicalSchema =
  FC.coerceSchema FC.text