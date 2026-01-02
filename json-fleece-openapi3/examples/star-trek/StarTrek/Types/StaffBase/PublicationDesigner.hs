{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.PublicationDesigner
  ( PublicationDesigner(..)
  , publicationDesignerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PublicationDesigner = PublicationDesigner Bool
  deriving (Show, Eq)

publicationDesignerSchema :: FC.Fleece t => FC.Schema t PublicationDesigner
publicationDesignerSchema =
  FC.coerceSchema FC.boolean