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
  { case_ :: Maybe Case.Case
  , class_ :: Maybe Class.Class
  , data_ :: Maybe Data.Data
  , deriving_ :: Maybe Deriving.Deriving
  , do_ :: Maybe Do.Do
  , else_ :: Maybe Else.Else
  , if_ :: Maybe If.If
  , import_ :: Maybe Import.Import
  , in_ :: Maybe In.In
  , infix_ :: Maybe Infix.Infix
  , infixl_ :: Maybe Infixl.Infixl
  , infixr_ :: Maybe Infixr.Infixr
  , instance_ :: Maybe Instance.Instance
  , int32 :: Maybe Int32.Int32
  , int64 :: Maybe Int64.Int64
  , let_ :: Maybe Let.Let
  , module_ :: Maybe Module.Module
  , nameConflicts :: Maybe NameConflicts.NameConflicts
  , newtype_ :: Maybe Newtype.Newtype
  , of_ :: Maybe Of.Of
  , scientific :: Maybe Scientific.Scientific
  , text :: Maybe Text.Text
  , then_ :: Maybe Then.Then
  , type_ :: Maybe Type.Type
  , where_ :: Maybe Where.Where
  }
  deriving (Eq, Show)

nameConflictsSchema :: FC.Fleece schema => schema NameConflicts
nameConflictsSchema =
  FC.object $
    FC.constructor NameConflicts
      #+ FC.optional "case" case_ Case.caseSchema
      #+ FC.optional "class" class_ Class.classSchema
      #+ FC.optional "data" data_ Data.dataSchema
      #+ FC.optional "deriving" deriving_ Deriving.derivingSchema
      #+ FC.optional "do" do_ Do.doSchema
      #+ FC.optional "else" else_ Else.elseSchema
      #+ FC.optional "if" if_ If.ifSchema
      #+ FC.optional "import" import_ Import.importSchema
      #+ FC.optional "in" in_ In.inSchema
      #+ FC.optional "infix" infix_ Infix.infixSchema
      #+ FC.optional "infixl" infixl_ Infixl.infixlSchema
      #+ FC.optional "infixr" infixr_ Infixr.infixrSchema
      #+ FC.optional "instance" instance_ Instance.instanceSchema
      #+ FC.optional "int32" int32 Int32.int32Schema
      #+ FC.optional "int64" int64 Int64.int64Schema
      #+ FC.optional "let" let_ Let.letSchema
      #+ FC.optional "module" module_ Module.moduleSchema
      #+ FC.optional "nameConflicts" nameConflicts NameConflicts.nameConflictsSchema
      #+ FC.optional "newtype" newtype_ Newtype.newtypeSchema
      #+ FC.optional "of" of_ Of.ofSchema
      #+ FC.optional "scientific" scientific Scientific.scientificSchema
      #+ FC.optional "text" text Text.textSchema
      #+ FC.optional "then" then_ Then.thenSchema
      #+ FC.optional "type" type_ Type.typeSchema
      #+ FC.optional "where" where_ Where.whereSchema