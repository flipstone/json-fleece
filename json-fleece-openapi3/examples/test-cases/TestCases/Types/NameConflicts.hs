{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NameConflicts
  ( NameConflicts(..)
  , nameConflictsSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.NameConflicts.Case as Case
import qualified TestCases.Types.NameConflicts.Class as Class
import qualified TestCases.Types.NameConflicts.Data as Data
import qualified TestCases.Types.NameConflicts.Deriving as Deriving
import qualified TestCases.Types.NameConflicts.Do as Do
import qualified TestCases.Types.NameConflicts.Else as Else
import qualified TestCases.Types.NameConflicts.If as If
import qualified TestCases.Types.NameConflicts.Import as Import
import qualified TestCases.Types.NameConflicts.In as In
import qualified TestCases.Types.NameConflicts.Infix as Infix
import qualified TestCases.Types.NameConflicts.Infixl as Infixl
import qualified TestCases.Types.NameConflicts.Infixr as Infixr
import qualified TestCases.Types.NameConflicts.Instance as Instance
import qualified TestCases.Types.NameConflicts.Int32 as Int32
import qualified TestCases.Types.NameConflicts.Int64 as Int64
import qualified TestCases.Types.NameConflicts.Let as Let
import qualified TestCases.Types.NameConflicts.Module as Module
import qualified TestCases.Types.NameConflicts.NameConflicts as NameConflicts
import qualified TestCases.Types.NameConflicts.Newtype as Newtype
import qualified TestCases.Types.NameConflicts.Of as Of
import qualified TestCases.Types.NameConflicts.Scientific as Scientific
import qualified TestCases.Types.NameConflicts.Text as Text
import qualified TestCases.Types.NameConflicts.Then as Then
import qualified TestCases.Types.NameConflicts.Type as Type
import qualified TestCases.Types.NameConflicts.Where as Where

data NameConflicts = NameConflicts
  { infixr_ :: Maybe Infixr.Infixr
  , let_ :: Maybe Let.Let
  , in_ :: Maybe In.In
  , text :: Maybe Text.Text
  , where_ :: Maybe Where.Where
  , case_ :: Maybe Case.Case
  , deriving_ :: Maybe Deriving.Deriving
  , data_ :: Maybe Data.Data
  , infix_ :: Maybe Infix.Infix
  , if_ :: Maybe If.If
  , else_ :: Maybe Else.Else
  , nameConflicts :: Maybe NameConflicts.NameConflicts
  , module_ :: Maybe Module.Module
  , then_ :: Maybe Then.Then
  , newtype_ :: Maybe Newtype.Newtype
  , scientific :: Maybe Scientific.Scientific
  , type_ :: Maybe Type.Type
  , import_ :: Maybe Import.Import
  , do_ :: Maybe Do.Do
  , instance_ :: Maybe Instance.Instance
  , int32 :: Maybe Int32.Int32
  , class_ :: Maybe Class.Class
  , of_ :: Maybe Of.Of
  , int64 :: Maybe Int64.Int64
  , infixl_ :: Maybe Infixl.Infixl
  }
  deriving (Eq, Show)

nameConflictsSchema :: FC.Fleece schema => schema NameConflicts
nameConflictsSchema =
  FC.object $
    FC.constructor NameConflicts
      #+ FC.optional "infixr" infixr_ Infixr.infixrSchema
      #+ FC.optional "let" let_ Let.letSchema
      #+ FC.optional "in" in_ In.inSchema
      #+ FC.optional "text" text Text.textSchema
      #+ FC.optional "where" where_ Where.whereSchema
      #+ FC.optional "case" case_ Case.caseSchema
      #+ FC.optional "deriving" deriving_ Deriving.derivingSchema
      #+ FC.optional "data" data_ Data.dataSchema
      #+ FC.optional "infix" infix_ Infix.infixSchema
      #+ FC.optional "if" if_ If.ifSchema
      #+ FC.optional "else" else_ Else.elseSchema
      #+ FC.optional "nameConflicts" nameConflicts NameConflicts.nameConflictsSchema
      #+ FC.optional "module" module_ Module.moduleSchema
      #+ FC.optional "then" then_ Then.thenSchema
      #+ FC.optional "newtype" newtype_ Newtype.newtypeSchema
      #+ FC.optional "scientific" scientific Scientific.scientificSchema
      #+ FC.optional "type" type_ Type.typeSchema
      #+ FC.optional "import" import_ Import.importSchema
      #+ FC.optional "do" do_ Do.doSchema
      #+ FC.optional "instance" instance_ Instance.instanceSchema
      #+ FC.optional "int32" int32 Int32.int32Schema
      #+ FC.optional "class" class_ Class.classSchema
      #+ FC.optional "of" of_ Of.ofSchema
      #+ FC.optional "int64" int64 Int64.int64Schema
      #+ FC.optional "infixl" infixl_ Infixl.infixlSchema