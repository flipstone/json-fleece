{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.EpisodeBase.FinalScriptDate
  ( FinalScriptDate(..)
  , finalScriptDateSchema
  ) where

import Data.Time (Day)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype FinalScriptDate = FinalScriptDate Day
  deriving (Show, Eq)

finalScriptDateSchema :: FC.Fleece schema => schema FinalScriptDate
finalScriptDateSchema =
  FC.coerceSchema FC.day