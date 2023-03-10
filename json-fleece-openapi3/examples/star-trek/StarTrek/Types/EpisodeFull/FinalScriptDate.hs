{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.EpisodeFull.FinalScriptDate
  ( FinalScriptDate(..)
  , finalScriptDateSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype FinalScriptDate = FinalScriptDate Time.Day
  deriving (Show, Eq)

finalScriptDateSchema :: FC.Fleece schema => schema FinalScriptDate
finalScriptDateSchema =
  FC.coerceSchema FC.day