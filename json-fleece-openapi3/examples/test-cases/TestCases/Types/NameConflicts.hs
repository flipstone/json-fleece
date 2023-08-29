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
  { int64 :: Maybe Int64.Int64
  , nameConflicts :: Maybe NameConflicts.NameConflicts
  , int32 :: Maybe Int32.Int32
  , case_ :: Maybe Case.Case
  , of_ :: Maybe Of.Of
  , then_ :: Maybe Then.Then
  , deriving_ :: Maybe Deriving.Deriving
  , let_ :: Maybe Let.Let
  , where_ :: Maybe Where.Where
  , data_ :: Maybe Data.Data
  , module_ :: Maybe Module.Module
  , text :: Maybe Text.Text
  , do_ :: Maybe Do.Do
  , else_ :: Maybe Else.Else
  , infixr_ :: Maybe Infixr.Infixr
  , import_ :: Maybe Import.Import
  , scientific :: Maybe Scientific.Scientific
  , infixl_ :: Maybe Infixl.Infixl
  , if_ :: Maybe If.If
  , newtype_ :: Maybe Newtype.Newtype
  , class_ :: Maybe Class.Class
  , type_ :: Maybe Type.Type
  , in_ :: Maybe In.In
  , instance_ :: Maybe Instance.Instance
  , infix_ :: Maybe Infix.Infix
  }
  deriving (Eq, Show)

nameConflictsSchema :: FC.Fleece schema => schema NameConflicts
nameConflictsSchema =
  FC.object $
    FC.constructor NameConflicts
      #+ FC.optional "int64" int64 Int64.int64Schema
      #+ FC.optional "nameConflicts" nameConflicts NameConflicts.nameConflictsSchema
      #+ FC.optional "int32" int32 Int32.int32Schema
      #+ FC.optional "case" case_ Case.caseSchema
      #+ FC.optional "of" of_ Of.ofSchema
      #+ FC.optional "then" then_ Then.thenSchema
      #+ FC.optional "deriving" deriving_ Deriving.derivingSchema
      #+ FC.optional "let" let_ Let.letSchema
      #+ FC.optional "where" where_ Where.whereSchema
      #+ FC.optional "data" data_ Data.dataSchema
      #+ FC.optional "module" module_ Module.moduleSchema
      #+ FC.optional "text" text Text.textSchema
      #+ FC.optional "do" do_ Do.doSchema
      #+ FC.optional "else" else_ Else.elseSchema
      #+ FC.optional "infixr" infixr_ Infixr.infixrSchema
      #+ FC.optional "import" import_ Import.importSchema
      #+ FC.optional "scientific" scientific Scientific.scientificSchema
      #+ FC.optional "infixl" infixl_ Infixl.infixlSchema
      #+ FC.optional "if" if_ If.ifSchema
      #+ FC.optional "newtype" newtype_ Newtype.newtypeSchema
      #+ FC.optional "class" class_ Class.classSchema
      #+ FC.optional "type" type_ Type.typeSchema
      #+ FC.optional "in" in_ In.inSchema
      #+ FC.optional "instance" instance_ Instance.instanceSchema
      #+ FC.optional "infix" infix_ Infix.infixSchema