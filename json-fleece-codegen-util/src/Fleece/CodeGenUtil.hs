{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Fleece.CodeGenUtil
  ( generateFleeceCode
  , CodeGenOptions
    ( CodeGenOptions
    , moduleBaseName
    )
  , CodeGen
  , runCodeGen
  , codeGenError
  , CodeGenResult
  , CodeGenError
  , CodeGenType (..)
  , CodeGenDataFormat (..)
  , CodeGenObjectField (..)
  , CodeGenObjectFieldType (..)
  , TypeMap
  , SchemaTypeInfo (..)
  , inferSchemaInfoForInputName
  , arrayTypeInfo
  , nullableTypeInfo
  , textFormat
  , boolFormat
  , int32Format
  , int64Format
  , integerFormat
  , scientificFormat
  , floatFormat
  , doubleFormat
  , dayFormat
  , utcTimeFormat
  , enumFormat
  , nullFormat
  , HC.HaskellCode
  , HC.renderLazyText
  , HC.renderText
  , Modules
  ) where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified Fleece.CodeGenUtil.HaskellCode as HC

newtype CodeGenError = CodeGenError String
  deriving (Show)

type CodeGen = ReaderT CodeGenOptions (Either CodeGenError)
type CodeGenResult = Either CodeGenError

runCodeGen :: CodeGenOptions -> CodeGen a -> CodeGenResult a
runCodeGen =
  flip runReaderT

type Modules =
  [(FilePath, HC.HaskellCode)]

data CodeGenOptions = CodeGenOptions
  { moduleBaseName :: T.Text
  }

codeGenError :: String -> CodeGen a
codeGenError = lift . Left . CodeGenError

type TypeMap =
  Map.Map T.Text CodeGenType

data CodeGenType = CodeGenType
  { codeGenTypeOriginalName :: T.Text
  , codeGenTypeSchemaInfo :: SchemaTypeInfo
  , codeGenTypeDescription :: Maybe T.Text
  , codeGenTypeDataFormat :: CodeGenDataFormat
  }

data CodeGenDataFormat
  = CodeGenNewType SchemaTypeInfo
  | CodeGenEnum [T.Text]
  | CodeGenObject [CodeGenObjectField]
  | CodeGenArray CodeGenObjectFieldType

data CodeGenObjectField = CodeGenObjectField
  { codeGenFieldName :: T.Text
  , codeGenFieldType :: CodeGenObjectFieldType
  , codeGenFieldRequired :: Bool
  }

data CodeGenObjectFieldType
  = TypeReference T.Text
  | CodeGenFieldArray
      Bool
      -- ^ whether the array itself is nullable
      CodeGenObjectFieldType

resolveFieldTypeInfo ::
  TypeMap ->
  CodeGenObjectFieldType ->
  CodeGen SchemaTypeInfo
resolveFieldTypeInfo typeMap =
  let
    go fieldType =
      case fieldType of
        TypeReference ref ->
          case Map.lookup ref typeMap of
            Nothing -> codeGenError $ "Type " <> show ref <> " not found"
            Just codeGenType -> pure (codeGenTypeSchemaInfo codeGenType)
        CodeGenFieldArray nullable itemType ->
          let
            modifier =
              if nullable
                then nullableTypeInfo
                else id
          in
            fmap (modifier . arrayTypeInfo) (go itemType)
  in
    go

resolveFieldDescription ::
  TypeMap ->
  CodeGenObjectFieldType ->
  Maybe T.Text
resolveFieldDescription typeMap =
  let
    go fieldType =
      case fieldType of
        TypeReference ref ->
          codeGenTypeDescription =<< Map.lookup ref typeMap
        CodeGenFieldArray _nullable itemType ->
          go itemType
  in
    go

dayFormat :: CodeGenDataFormat
dayFormat =
  CodeGenNewType $
    SchemaTypeInfo
      { schemaTypeName = HC.toTypeName "Data.Time" "Day"
      , schemaTypeSchema = fleeceCoreVar "day"
      }

utcTimeFormat :: CodeGenDataFormat
utcTimeFormat =
  CodeGenNewType $
    SchemaTypeInfo
      { schemaTypeName = HC.toTypeName "Data.Time" "UTCTime"
      , schemaTypeSchema = fleeceCoreVar "utcTime"
      }

textFormat :: CodeGenDataFormat
textFormat =
  CodeGenNewType $
    SchemaTypeInfo
      { schemaTypeName = textType
      , schemaTypeSchema = fleeceCoreVar "text"
      }

floatFormat :: CodeGenDataFormat
floatFormat =
  CodeGenNewType $
    SchemaTypeInfo
      { schemaTypeName = floatType
      , schemaTypeSchema = fleeceCoreVar "float"
      }

doubleFormat :: CodeGenDataFormat
doubleFormat =
  CodeGenNewType $
    SchemaTypeInfo
      { schemaTypeName = doubleType
      , schemaTypeSchema = fleeceCoreVar "double"
      }

scientificFormat :: CodeGenDataFormat
scientificFormat =
  CodeGenNewType $
    SchemaTypeInfo
      { schemaTypeName = scientificType
      , schemaTypeSchema = fleeceCoreVar "number"
      }

int32Format :: CodeGenDataFormat
int32Format =
  CodeGenNewType $
    SchemaTypeInfo
      { schemaTypeName = int32Type
      , schemaTypeSchema = fleeceCoreVar "int32"
      }

int64Format :: CodeGenDataFormat
int64Format =
  CodeGenNewType $
    SchemaTypeInfo
      { schemaTypeName = int64Type
      , schemaTypeSchema = fleeceCoreVar "int64"
      }

integerFormat :: CodeGenDataFormat
integerFormat =
  CodeGenNewType $
    SchemaTypeInfo
      { schemaTypeName = integerType
      , schemaTypeSchema = fleeceCoreVar "integer"
      }

boolFormat :: CodeGenDataFormat
boolFormat =
  CodeGenNewType $
    SchemaTypeInfo
      { schemaTypeName = boolType
      , schemaTypeSchema = fleeceCoreVar "boolean"
      }

enumFormat :: [T.Text] -> CodeGenDataFormat
enumFormat =
  CodeGenEnum

nullFormat :: CodeGenDataFormat
nullFormat =
  CodeGenNewType $
    SchemaTypeInfo
      { schemaTypeName = fleeceCoreType "Null"
      , schemaTypeSchema = fleeceCoreVar "null"
      }

arrayTypeInfo :: SchemaTypeInfo -> SchemaTypeInfo
arrayTypeInfo itemInfo =
  SchemaTypeInfo
    { schemaTypeName = HC.listOf (schemaTypeName itemInfo)
    , schemaTypeSchema =
        "("
          <> fleeceCoreVar "list"
          <> " "
          <> schemaTypeSchema itemInfo
          <> ")"
    }

nullableTypeInfo :: SchemaTypeInfo -> SchemaTypeInfo
nullableTypeInfo itemInfo =
  SchemaTypeInfo
    { schemaTypeName = HC.eitherOf (fleeceCoreType "Null") (schemaTypeName itemInfo)
    , schemaTypeSchema =
        "("
          <> fleeceCoreVar "nullable"
          <> " "
          <> schemaTypeSchema itemInfo
          <> ")"
    }

generateFleeceCode :: TypeMap -> CodeGen Modules
generateFleeceCode typeMap =
  traverse (generateSchemaCode typeMap) (Map.elems typeMap)

generateSchemaCode ::
  TypeMap ->
  CodeGenType ->
  CodeGen (FilePath, HC.HaskellCode)
generateSchemaCode typeMap codeGenType = do
  (moduleName, typeName) <-
    inferTypeForInputName (codeGenTypeOriginalName codeGenType)

  let
    path =
      T.unpack (T.replace "." "/" (HC.moduleNameToText moduleName) <> ".hs")

    format =
      codeGenTypeDataFormat codeGenType

  moduleBody <-
    case format of
      CodeGenNewType baseTypeInfo ->
        generateHaskellNewtype
          typeName
          (schemaTypeName baseTypeInfo)
          (schemaTypeSchema baseTypeInfo)
      CodeGenEnum values ->
        generateHaskellEnum typeName values
      CodeGenObject fields ->
        generateHaskellObject typeMap typeName fields
      CodeGenArray itemType ->
        generateHaskellArray typeMap typeName itemType

  header <- moduleHeader moduleName typeName

  let
    code =
      HC.declarations $
        [ "{-# LANGUAGE NoImplicitPrelude #-}"
        , header
        , importDeclarations moduleName moduleBody
        , (moduleBody)
        ]

  pure (path, code)

moduleHeader :: HC.ModuleName -> HC.TypeName -> CodeGen HC.HaskellCode
moduleHeader moduleName typeName = do
  schemaName <- fleeceSchemaNameForType typeName
  pure $
    HC.lines
      [ "module " <> HC.toCode moduleName
      , HC.indent 2 ("( " <> HC.toCode typeName <> "(..)")
      , HC.indent 2 (", " <> HC.toCode schemaName)
      , HC.indent 2 ") where"
      ]

data ImportItems = ImportItems
  { unqualifiedImports :: Set.Set T.Text
  , qualifiers :: Set.Set T.Text
  }

importName :: T.Text -> ImportItems
importName name =
  ImportItems
    { unqualifiedImports = Set.singleton name
    , qualifiers = mempty
    }

importQualifier :: T.Text -> ImportItems
importQualifier qualifier =
  ImportItems
    { unqualifiedImports = mempty
    , qualifiers = Set.singleton qualifier
    }

unionImports :: ImportItems -> ImportItems -> ImportItems
unionImports left right =
  ImportItems
    { unqualifiedImports = unqualifiedImports left <> unqualifiedImports right
    , qualifiers = qualifiers left <> qualifiers right
    }

importDeclarations ::
  HC.ModuleName ->
  HC.HaskellCode ->
  HC.HaskellCode
importDeclarations thisModuleName code =
  let
    mkImport :: HC.ExternalReference -> Map.Map HC.ModuleName ImportItems
    mkImport ref =
      case ref of
        HC.TypeReference moduleName typeName ->
          if moduleName == thisModuleName
            then Map.empty
            else Map.singleton moduleName (importName typeName)
        HC.VarReference moduleName varName ->
          if moduleName == thisModuleName
            then Map.empty
            else Map.singleton moduleName (importName varName)
        HC.QualifierReference moduleName qualifier ->
          Map.singleton moduleName (importQualifier qualifier)

    imports =
      Map.unionsWith
        unionImports
        (map mkImport . Set.toList . HC.references $ code)

    importNames moduleName names =
      "import "
        <> HC.toCode moduleName
        <> " ("
        <> HC.intercalate ", " (map HC.fromText (Set.toList names))
        <> ")"

    importQualifiedAs moduleName qualifier =
      "import qualified "
        <> HC.toCode moduleName
        <> " as "
        <> HC.fromText qualifier

    mkImportLines (moduleName, importItems) =
      importNames moduleName (unqualifiedImports importItems)
        : map (importQualifiedAs moduleName) (Set.toList (qualifiers importItems))

    importLines =
      foldMap mkImportLines (Map.toList imports)
  in
    HC.lines importLines

generateHaskellNewtype ::
  HC.TypeName ->
  HC.TypeName ->
  HC.HaskellCode ->
  CodeGen HC.HaskellCode
generateHaskellNewtype wrapperName baseName schemaName = do
  let
    newtypeDecl =
      HC.lines
        [ "newtype "
            <> HC.toCode wrapperName
            <> " = "
            <> HC.toCode wrapperName
            <> " "
            <> HC.toCode baseName
        , HC.indent 2 (HC.deriving_ [HC.showClass, HC.eqClass])
        ]

  fleeceSchema <-
    fleeceSchemaForType wrapperName $
      [ fleeceCoreVar "coerceSchema" <> " " <> schemaName
      ]

  pure $
    HC.declarations
      [ newtypeDecl
      , fleeceSchema
      ]

generateHaskellEnum :: HC.TypeName -> [T.Text] -> CodeGen HC.HaskellCode
generateHaskellEnum typeName enumValues = do
  let
    mkEnumItem t =
      (t, HC.toConstructorName t)

    enumItems =
      map mkEnumItem enumValues

  moduleName <- moduleNameForType typeName

  let
    enumDeclaration =
      HC.enum typeName (map snd enumItems)

    toTextName =
      HC.toVarName
        moduleName
        (HC.renderText typeName <> "ToText")

    toText =
      HC.lines
        ( HC.typeAnnotate toTextName (HC.toCode typeName <> " -> " <> HC.toCode textType)
            : HC.toCode toTextName <> " v ="
            : HC.indent 2 (HC.toCode (HC.toVarName "Data.Text" "pack") <> " " <> HC.dollar)
            : HC.indent 4 "case v of"
            : map (HC.indent 6 . mkToTextCase) enumItems
        )

    mkToTextCase (text, constructor) =
      HC.caseMatch constructor (HC.stringLiteral text)

  fleeceSchema <-
    fleeceSchemaForType typeName $
      [ fleeceCoreVar "boundedEnum" <> " " <> HC.toCode toTextName
      ]

  pure $
    HC.declarations
      [ enumDeclaration
      , toText
      , fleeceSchema
      ]

generateHaskellObject ::
  TypeMap ->
  HC.TypeName ->
  [CodeGenObjectField] ->
  CodeGen HC.HaskellCode
generateHaskellObject typeMap typeName codeGenFields = do
  moduleName <- moduleNameForType typeName

  fields <-
    traverse (mkFleeceSchemaField typeMap moduleName) codeGenFields

  let
    fieldNameAndType field =
      (fieldName field, fieldTypeName field, fieldDescription field)

    recordDecl =
      HC.record typeName (map fieldNameAndType fields)

    fleeceField field =
      HC.addReferences [HC.VarReference "Fleece.Core" "(#+)"] $
        "#+ "
          <> fleeceFieldFunction field
          <> " "
          <> HC.stringLiteral (fieldJSONName field)
          <> " "
          <> HC.toCode (fieldName field)
          <> " "
          <> fieldFleeceSchemaCode field

  fleeceSchema <-
    fleeceSchemaForType typeName $
      ( fleeceCoreVar "object" <> " " <> HC.dollar
          : "  " <> fleeceCoreVar "constructor" <> " " <> HC.toCode typeName
          : map (HC.indent 4 . fleeceField) fields
      )

  pure $
    HC.declarations
      [ recordDecl
      , fleeceSchema
      ]

generateHaskellArray ::
  TypeMap ->
  HC.TypeName ->
  CodeGenObjectFieldType ->
  CodeGen HC.HaskellCode
generateHaskellArray typeMap typeName itemType = do
  typeInfo <- fmap arrayTypeInfo (resolveFieldTypeInfo typeMap itemType)
  generateHaskellNewtype
    typeName
    (schemaTypeName typeInfo)
    (schemaTypeSchema typeInfo)

data FleeceSchemaField = FleeceSchemaField
  { fieldName :: HC.VarName
  , fieldJSONName :: T.Text
  , fieldCodeGenField :: CodeGenObjectField
  , fieldBaseSchemaTypeInfo :: SchemaTypeInfo
  , fieldDescription :: Maybe T.Text
  }

fieldRequired :: FleeceSchemaField -> Bool
fieldRequired =
  codeGenFieldRequired . fieldCodeGenField

fieldBaseTypeName :: FleeceSchemaField -> HC.TypeName
fieldBaseTypeName =
  schemaTypeName . fieldBaseSchemaTypeInfo

fieldTypeName :: FleeceSchemaField -> HC.TypeName
fieldTypeName field =
  if fieldRequired field
    then fieldBaseTypeName field
    else HC.maybeOf (fieldBaseTypeName field)

fieldFleeceSchemaCode :: FleeceSchemaField -> HC.HaskellCode
fieldFleeceSchemaCode =
  schemaTypeSchema . fieldBaseSchemaTypeInfo

fleeceFieldFunction :: FleeceSchemaField -> HC.HaskellCode
fleeceFieldFunction field =
  if fieldRequired field
    then fleeceCoreVar "required"
    else fleeceCoreVar "optional"

mkFleeceSchemaField ::
  TypeMap ->
  HC.ModuleName ->
  CodeGenObjectField ->
  CodeGen FleeceSchemaField
mkFleeceSchemaField typeMap moduleName codeGenField = do
  let
    name =
      codeGenFieldName codeGenField

    fieldType =
      codeGenFieldType codeGenField

  typeInfo <- resolveFieldTypeInfo typeMap fieldType

  pure $
    FleeceSchemaField
      { fieldName = HC.toVarName moduleName name
      , fieldCodeGenField = codeGenField
      , fieldBaseSchemaTypeInfo = typeInfo
      , fieldJSONName = name
      , fieldDescription = resolveFieldDescription typeMap fieldType
      }

data SchemaTypeInfo = SchemaTypeInfo
  { schemaTypeName :: HC.TypeName
  , schemaTypeSchema :: HC.HaskellCode
  }

inferSchemaInfoForInputName :: T.Text -> CodeGen SchemaTypeInfo
inferSchemaInfoForInputName name = do
  (_moduleName, typeName) <- inferTypeForInputName name

  schema <- fleeceSchemaNameForType typeName

  pure $
    SchemaTypeInfo
      { schemaTypeName = typeName
      , schemaTypeSchema = HC.toCode schema
      }

inferTypeForInputName :: T.Text -> (CodeGen (HC.ModuleName, HC.TypeName))
inferTypeForInputName inputName = do
  moduleName <- generatedModuleName inputName
  case reverse (T.splitOn "." inputName) of
    [] ->
      codeGenError $ "Unable to determine type name for: " <> show inputName
    ("" : _) ->
      codeGenError $ "Unable to determine type name for: " <> show inputName
    (lastPart : _) ->
      pure (moduleName, HC.toTypeName moduleName lastPart)

fleeceSchemaNameForType :: HC.TypeName -> CodeGen HC.VarName
fleeceSchemaNameForType typeName = do
  moduleName <- moduleNameForType typeName
  pure $
    HC.toVarName
      moduleName
      (HC.renderText typeName <> "Schema")

fleeceSchemaForType :: HC.TypeName -> [HC.HaskellCode] -> CodeGen HC.HaskellCode
fleeceSchemaForType typeName bodyLines = do
  schemaName <- fleeceSchemaNameForType typeName

  let
    declType =
      HC.typeAnnotate schemaName $
        HC.toCode fleeceClass
          <> " schema => schema "
          <> HC.toCode typeName

    declImpl =
      HC.toCode schemaName <> " ="

  pure $
    HC.lines
      ( declType
          : declImpl
          : map (HC.indent 2) bodyLines
      )

moduleNameForType :: HC.TypeName -> CodeGen HC.ModuleName
moduleNameForType typeName =
  let
    typeText =
      HC.renderText typeName

    refs =
      HC.references typeName

    matchingRefModuleName ref =
      case ref of
        HC.TypeReference moduleName referencedType ->
          if typeText == referencedType
            then Just moduleName
            else Nothing
        _ ->
          Nothing
  in
    case mapMaybe matchingRefModuleName (Set.toList refs) of
      (moduleName : _) -> pure moduleName
      _ -> codeGenError $ "Unable to determine module for type: " <> show typeText

generatedModuleName :: T.Text -> CodeGen HC.ModuleName
generatedModuleName text = do
  options <- ask
  pure $
    HC.ModuleName (moduleBaseName options) <> HC.toModuleName text

textType :: HC.TypeName
textType =
  HC.toTypeName "Data.Text" "Text"

floatType :: HC.TypeName
floatType =
  preludeType "Float"

doubleType :: HC.TypeName
doubleType =
  preludeType "Double"

scientificType :: HC.TypeName
scientificType =
  HC.toTypeName "Data.Scientific" "Scientific"

int32Type :: HC.TypeName
int32Type =
  HC.toTypeName "Data.Int" "Int32"

int64Type :: HC.TypeName
int64Type =
  HC.toTypeName "Data.Int" "Int64"

integerType :: HC.TypeName
integerType =
  preludeType "Integer"

boolType :: HC.TypeName
boolType =
  preludeType "Bool"

preludeType :: T.Text -> HC.TypeName
preludeType =
  HC.toTypeName "Prelude"

fleeceClass :: HC.TypeName
fleeceClass =
  fleeceCoreType "Fleece"

fleeceCoreType :: T.Text -> HC.TypeName
fleeceCoreType =
  HC.toQualifiedTypeName "Fleece.Core" "FC"

fleeceCoreVar :: HC.FromCode c => T.Text -> c
fleeceCoreVar =
  HC.fromCode . HC.toCode . HC.toQualifiedVarName "Fleece.Core" "FC"
