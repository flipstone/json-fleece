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
  { else_ :: Maybe Else.Else
  , infix_ :: Maybe Infix.Infix
  , import_ :: Maybe Import.Import
  , then_ :: Maybe Then.Then
  , type_ :: Maybe Type.Type
  , let_ :: Maybe Let.Let
  , deriving_ :: Maybe Deriving.Deriving
  , infixl_ :: Maybe Infixl.Infixl
  , case_ :: Maybe Case.Case
  , class_ :: Maybe Class.Class
  , if_ :: Maybe If.If
  , where_ :: Maybe Where.Where
  , of_ :: Maybe Of.Of
  , nameConflicts :: Maybe NameConflicts.NameConflicts
  , int32 :: Maybe Int32.Int32
  , in_ :: Maybe In.In
  , module_ :: Maybe Module.Module
  , infixr_ :: Maybe Infixr.Infixr
  , data_ :: Maybe Data.Data
  , newtype_ :: Maybe Newtype.Newtype
  , instance_ :: Maybe Instance.Instance
  , do_ :: Maybe Do.Do
  , scientific :: Maybe Scientific.Scientific
  , text :: Maybe Text.Text
  , int64 :: Maybe Int64.Int64
  }
  deriving (Eq, Show)

nameConflictsSchema :: FC.Fleece schema => schema NameConflicts
nameConflictsSchema =
  FC.object $
    FC.constructor NameConflicts
      #+ FC.optional "else" else_ Else.elseSchema
      #+ FC.optional "infix" infix_ Infix.infixSchema
      #+ FC.optional "import" import_ Import.importSchema
      #+ FC.optional "then" then_ Then.thenSchema
      #+ FC.optional "type" type_ Type.typeSchema
      #+ FC.optional "let" let_ Let.letSchema
      #+ FC.optional "deriving" deriving_ Deriving.derivingSchema
      #+ FC.optional "infixl" infixl_ Infixl.infixlSchema
      #+ FC.optional "case" case_ Case.caseSchema
      #+ FC.optional "class" class_ Class.classSchema
      #+ FC.optional "if" if_ If.ifSchema
      #+ FC.optional "where" where_ Where.whereSchema
      #+ FC.optional "of" of_ Of.ofSchema
      #+ FC.optional "nameConflicts" nameConflicts NameConflicts.nameConflictsSchema
      #+ FC.optional "int32" int32 Int32.int32Schema
      #+ FC.optional "in" in_ In.inSchema
      #+ FC.optional "module" module_ Module.moduleSchema
      #+ FC.optional "infixr" infixr_ Infixr.infixrSchema
      #+ FC.optional "data" data_ Data.dataSchema
      #+ FC.optional "newtype" newtype_ Newtype.newtypeSchema
      #+ FC.optional "instance" instance_ Instance.instanceSchema
      #+ FC.optional "do" do_ Do.doSchema
      #+ FC.optional "scientific" scientific Scientific.scientificSchema
      #+ FC.optional "text" text Text.textSchema
      #+ FC.optional "int64" int64 Int64.int64Schema