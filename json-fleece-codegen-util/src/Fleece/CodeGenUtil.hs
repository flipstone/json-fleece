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
    primitiveSchemaTypeInfo
      (HC.toTypeName "Data.Time" (Just "Time") "Day")
      (fleeceCoreVar "day")

utcTimeFormat :: CodeGenDataFormat
utcTimeFormat =
  CodeGenNewType $
    primitiveSchemaTypeInfo
      (HC.toTypeName "Data.Time" (Just "Time") "UTCTime")
      (fleeceCoreVar "utcTime")

textFormat :: CodeGenDataFormat
textFormat =
  CodeGenNewType $
    primitiveSchemaTypeInfo textType (fleeceCoreVar "text")

floatFormat :: CodeGenDataFormat
floatFormat =
  CodeGenNewType $
    primitiveSchemaTypeInfo floatType (fleeceCoreVar "float")

doubleFormat :: CodeGenDataFormat
doubleFormat =
  CodeGenNewType $
    primitiveSchemaTypeInfo doubleType (fleeceCoreVar "double")

scientificFormat :: CodeGenDataFormat
scientificFormat =
  CodeGenNewType $
    primitiveSchemaTypeInfo scientificType (fleeceCoreVar "number")

int32Format :: CodeGenDataFormat
int32Format =
  CodeGenNewType $
    primitiveSchemaTypeInfo int32Type (fleeceCoreVar "int32")

int64Format :: CodeGenDataFormat
int64Format =
  CodeGenNewType $
    primitiveSchemaTypeInfo int64Type (fleeceCoreVar "int64")

integerFormat :: CodeGenDataFormat
integerFormat =
  CodeGenNewType $
    primitiveSchemaTypeInfo integerType (fleeceCoreVar "integer")

boolFormat :: CodeGenDataFormat
boolFormat =
  CodeGenNewType $
    primitiveSchemaTypeInfo boolType (fleeceCoreVar "boolean")

enumFormat :: [T.Text] -> CodeGenDataFormat
enumFormat =
  CodeGenEnum

nullFormat :: CodeGenDataFormat
nullFormat =
  CodeGenNewType $
    primitiveSchemaTypeInfo (fleeceCoreType "Null") (fleeceCoreVar "null")

arrayTypeInfo :: SchemaTypeInfo -> SchemaTypeInfo
arrayTypeInfo itemInfo =
  itemInfo
    { schemaTypeExpr = HC.listOf (schemaTypeExpr itemInfo)
    , schemaTypeSchema =
        "("
          <> fleeceCoreVar "list"
          <> " "
          <> schemaTypeSchema itemInfo
          <> ")"
    }

nullableTypeInfo :: SchemaTypeInfo -> SchemaTypeInfo
nullableTypeInfo itemInfo =
  itemInfo
    { schemaTypeExpr =
        HC.eitherOf
          (HC.typeNameToCodeDefaultQualification (fleeceCoreType "Null"))
          (schemaTypeExpr itemInfo)
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

    header =
      moduleHeader moduleName typeName

  moduleBody <-
    case format of
      CodeGenNewType baseTypeInfo ->
        pure $
          generateHaskellNewtype
            typeName
            (schemaTypeExpr baseTypeInfo)
            (schemaTypeSchema baseTypeInfo)
      CodeGenEnum values ->
        pure $ generateHaskellEnum typeName values
      CodeGenObject fields ->
        generateHaskellObject typeMap typeName fields
      CodeGenArray itemType ->
        generateHaskellArray typeMap typeName itemType

  let
    code =
      HC.declarations $
        [ "{-# LANGUAGE NoImplicitPrelude #-}"
        , header
        , importDeclarations moduleName moduleBody
        , (moduleBody)
        ]

  pure (path, code)

moduleHeader :: HC.ModuleName -> HC.TypeName -> HC.HaskellCode
moduleHeader moduleName typeName =
  let
    schemaName =
      fleeceSchemaNameForType typeName
  in
    HC.lines
      [ "module " <> HC.toCode moduleName
      , HC.indent 2 ("( " <> HC.typeNameToCode Nothing typeName <> "(..)")
      , HC.indent 2 (", " <> HC.varNameToCode Nothing schemaName)
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
        HC.TypeReference moduleName mbQualifier typeName ->
          if moduleName == thisModuleName
            then Map.empty
            else case mbQualifier of
              Nothing -> Map.singleton moduleName (importName typeName)
              Just qualifier -> Map.singleton moduleName (importQualifier qualifier)
        HC.VarReference moduleName mbQualifier varName ->
          if moduleName == thisModuleName
            then Map.empty
            else case mbQualifier of
              Nothing -> Map.singleton moduleName (importName varName)
              Just qualifier -> Map.singleton moduleName (importQualifier qualifier)

    imports =
      Map.unionsWith
        unionImports
        (map mkImport . Set.toList . HC.references $ code)

    importNames moduleName names =
      case Set.toList names of
        [] ->
          Nothing
        nonEmptyNames ->
          Just $
            "import "
              <> HC.toCode moduleName
              <> " ("
              <> HC.intercalate ", " (map HC.fromText nonEmptyNames)
              <> ")"

    importQualifiedAs moduleName qualifier =
      "import qualified "
        <> HC.toCode moduleName
        <> " as "
        <> HC.fromText qualifier

    mkImportLines (moduleName, importItems) =
      let
        mbUnqualifiedLine =
          importNames moduleName (unqualifiedImports importItems)
        qualifiedLines =
          map (importQualifiedAs moduleName) (Set.toList (qualifiers importItems))
      in
        case mbUnqualifiedLine of
          Nothing -> qualifiedLines
          Just qualifiedLine -> qualifiedLine : qualifiedLines

    importLines =
      foldMap mkImportLines (Map.toList imports)
  in
    HC.lines importLines

generateHaskellNewtype ::
  HC.TypeName ->
  HC.TypeExpression ->
  HC.HaskellCode ->
  HC.HaskellCode
generateHaskellNewtype wrapperName baseType schemaName =
  let
    newtypeDecl =
      HC.lines
        [ "newtype "
            <> HC.typeNameToCode Nothing wrapperName
            <> " = "
            <> HC.typeNameToCode Nothing wrapperName
            <> " "
            <> HC.toCode baseType
        , HC.indent 2 (HC.deriving_ [HC.showClass, HC.eqClass])
        ]

    fleeceSchema =
      fleeceSchemaForType wrapperName $
        [ fleeceCoreVar "coerceSchema" <> " " <> schemaName
        ]
  in
    HC.declarations
      [ newtypeDecl
      , fleeceSchema
      ]

generateHaskellEnum :: HC.TypeName -> [T.Text] -> HC.HaskellCode
generateHaskellEnum typeName enumValues =
  let
    mkEnumItem t =
      (t, HC.toConstructorName t)

    enumItems =
      map mkEnumItem enumValues

    moduleName =
      HC.typeNameModule typeName

    enumDeclaration =
      HC.enum typeName (map snd enumItems)

    toTextName =
      HC.toVarName
        moduleName
        Nothing
        (HC.typeNameText typeName <> "ToText")

    toTextType =
      HC.typeNameToCode Nothing typeName
        <> " -> "
        <> HC.typeNameToCodeDefaultQualification textType

    toText =
      HC.lines
        ( HC.typeAnnotate toTextName toTextType
            : HC.varNameToCode Nothing toTextName <> " v ="
            : HC.indent 2 (HC.varNameToCodeDefaultQualification textPack <> " " <> HC.dollar)
            : HC.indent 4 "case v of"
            : map (HC.indent 6 . mkToTextCase) enumItems
        )

    mkToTextCase (text, constructor) =
      HC.caseMatch constructor (HC.stringLiteral text)

    fleeceSchema =
      fleeceSchemaForType typeName $
        [ fleeceCoreVar "boundedEnum" <> " " <> HC.varNameToCode Nothing toTextName
        ]
  in
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
  let
    moduleName =
      HC.typeNameModule typeName

  fields <-
    traverse (mkFleeceSchemaField typeMap moduleName) codeGenFields

  let
    fieldNameAndType field =
      (fieldName field, fieldTypeName field, fieldDescription field)

    recordDecl =
      HC.record typeName (map fieldNameAndType fields)

    fleeceField field =
      HC.addReferences [HC.VarReference "Fleece.Core" Nothing "(#+)"] $
        "#+ "
          <> fleeceFieldFunction field
          <> " "
          <> HC.stringLiteral (fieldJSONName field)
          <> " "
          <> HC.varNameToCode Nothing (fieldName field)
          <> " "
          <> fieldFleeceSchemaCode field

    fleeceSchema =
      fleeceSchemaForType typeName $
        ( fleeceCoreVar "object" <> " " <> HC.dollar
            : "  " <> fleeceCoreVar "constructor" <> " " <> HC.typeNameToCode Nothing typeName
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
  pure $
    generateHaskellNewtype
      typeName
      (schemaTypeExpr typeInfo)
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

fieldBaseTypeExpr :: FleeceSchemaField -> HC.TypeExpression
fieldBaseTypeExpr =
  schemaTypeExpr . fieldBaseSchemaTypeInfo

fieldTypeName :: FleeceSchemaField -> HC.TypeExpression
fieldTypeName field =
  let
    baseTypeExpression =
      fieldBaseTypeExpr field
  in
    if fieldRequired field
      then baseTypeExpression
      else HC.maybeOf baseTypeExpression

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
      { fieldName = HC.toVarName moduleName Nothing name
      , fieldCodeGenField = codeGenField
      , fieldBaseSchemaTypeInfo = typeInfo
      , fieldJSONName = name
      , fieldDescription = resolveFieldDescription typeMap fieldType
      }

data SchemaTypeInfo = SchemaTypeInfo
  { schemaTypeExpr :: HC.TypeExpression
  , schemaTypeSchema :: HC.HaskellCode
  }

primitiveSchemaTypeInfo :: HC.TypeName -> HC.HaskellCode -> SchemaTypeInfo
primitiveSchemaTypeInfo typeName schema =
  SchemaTypeInfo
    { schemaTypeExpr = HC.typeNameToCodeDefaultQualification typeName
    , schemaTypeSchema = schema
    }

inferSchemaInfoForInputName :: T.Text -> CodeGen SchemaTypeInfo
inferSchemaInfoForInputName name = do
  (_moduleName, typeName) <- inferTypeForInputName name

  let
    schema =
      fleeceSchemaNameForType typeName

  pure $
    SchemaTypeInfo
      { schemaTypeExpr = HC.typeNameToCodeDefaultQualification typeName
      , schemaTypeSchema = HC.varNameToCodeDefaultQualification schema
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
      pure (moduleName, HC.toTypeName moduleName (Just lastPart) lastPart)

fleeceSchemaNameForType :: HC.TypeName -> HC.VarName
fleeceSchemaNameForType typeName =
  HC.toVarName
    (HC.typeNameModule typeName)
    (HC.typeNameSuggestedQualifier typeName)
    (HC.typeNameText typeName <> "Schema")

fleeceSchemaForType :: HC.TypeName -> [HC.HaskellCode] -> HC.HaskellCode
fleeceSchemaForType typeName bodyLines =
  let
    schemaName =
      fleeceSchemaNameForType typeName

    declType =
      HC.typeAnnotate schemaName $
        HC.typeNameToCodeDefaultQualification fleeceClass
          <> " schema => schema "
          <> HC.typeNameToCode Nothing typeName

    declImpl =
      HC.varNameToCode Nothing schemaName <> " ="
  in
    HC.lines
      ( declType
          : declImpl
          : map (HC.indent 2) bodyLines
      )

generatedModuleName :: T.Text -> CodeGen HC.ModuleName
generatedModuleName text = do
  options <- ask
  pure $
    HC.ModuleName (moduleBaseName options) <> HC.toModuleName text

textType :: HC.TypeName
textType =
  HC.toTypeName "Data.Text" (Just "T") "Text"

textPack :: HC.VarName
textPack =
  HC.toVarName "Data.Text" (Just "T") "pack"

floatType :: HC.TypeName
floatType =
  HC.preludeType "Float"

doubleType :: HC.TypeName
doubleType =
  HC.preludeType "Double"

scientificType :: HC.TypeName
scientificType =
  HC.toTypeName "Data.Scientific" (Just "Sci") "Scientific"

int32Type :: HC.TypeName
int32Type =
  HC.toTypeName "Data.Int" (Just "I") "Int32"

int64Type :: HC.TypeName
int64Type =
  HC.toTypeName "Data.Int" (Just "I") "Int64"

integerType :: HC.TypeName
integerType =
  HC.preludeType "Integer"

boolType :: HC.TypeName
boolType =
  HC.preludeType "Bool"

fleeceClass :: HC.TypeName
fleeceClass =
  fleeceCoreType "Fleece"

fleeceCoreType :: T.Text -> HC.TypeName
fleeceCoreType =
  HC.toTypeName "Fleece.Core" (Just "FC")

fleeceCoreVar :: HC.FromCode c => T.Text -> c
fleeceCoreVar =
  HC.fromCode
    . HC.varNameToCodeDefaultQualification
    . HC.toVarName "Fleece.Core" (Just "FC")
