{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.PublicationDesigner
  ( PublicationDesigner(..)
  , publicationDesignerSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PublicationDesigner = PublicationDesigner Bool
  deriving (Show, Eq)

publicationDesignerSchema :: FC.Fleece schema => schema PublicationDesigner
publicationDesignerSchema =
  FC.coerceSchema FC.boolean