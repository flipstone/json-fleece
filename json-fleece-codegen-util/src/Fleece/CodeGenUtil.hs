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
  , CodeGenItem (..)
  , CodeGenType (..)
  , CodeGenOperation (..)
  , CodeGenOperationParam (..)
  , OperationPathPiece (..)
  , OperationParamType (..)
  , CodeGenDataFormat (..)
  , CodeGenObjectField (..)
  , CodeGenObjectFieldType (..)
  , TypeMap
  , SchemaTypeInfo (..)
  , CodeSection (Type, Operation)
  , inferSchemaInfoForInputName
  , inferTypeForInputName
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
  Map.Map T.Text CodeGenItem

data CodeGenItem
  = CodeGenItemType CodeGenType
  | CodeGenItemOperation CodeGenOperation
  | CodeGenItemOperationParam CodeGenOperationParam

data CodeGenType = CodeGenType
  { codeGenTypeOriginalName :: T.Text
  , codeGenTypeSchemaInfo :: SchemaTypeInfo
  , codeGenTypeDescription :: Maybe T.Text
  , codeGenTypeDataFormat :: CodeGenDataFormat
  }

data CodeGenOperation = CodeGenOperation
  { codeGenOperationOriginalName :: T.Text
  , codeGenOperationMethod :: T.Text
  , codeGenOperationPath :: [OperationPathPiece]
  }

data CodeGenOperationParam = CodeGenOperationParam
  { codeGenOperationParamName :: T.Text
  , codeGenOperationParamTypeName :: HC.TypeName
  , codeGenOperationParamType :: OperationParamType
  }

data OperationPathPiece
  = PathLiteral T.Text
  | PathParamRef CodeGenOperationParam

data OperationParamType
  = ParamTypeString
  | ParamTypeInteger
  | ParamTypeInt
  | ParamTypeInt8
  | ParamTypeInt16
  | ParamTypeInt32
  | ParamTypeInt64
  | ParamTypeArray OperationParamType

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
            Just (CodeGenItemType codeGenType) ->
              pure (codeGenTypeSchemaInfo codeGenType)
            _ ->
              codeGenError $ "Type " <> show ref <> " not found."
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
          case Map.lookup ref typeMap of
            Just (CodeGenItemType codeGenType) ->
              codeGenTypeDescription codeGenType
            Just (CodeGenItemOperation _operation) ->
              Nothing
            Just (CodeGenItemOperationParam _param) ->
              Nothing
            Nothing ->
              Nothing
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
  traverse (generateItem typeMap) (Map.elems typeMap)

generateItem ::
  TypeMap ->
  CodeGenItem ->
  CodeGen (FilePath, HC.HaskellCode)
generateItem typeMap codeGenItem =
  case codeGenItem of
    CodeGenItemType codeGenType ->
      generateSchemaCode typeMap codeGenType
    CodeGenItemOperation codeGenOperation ->
      generateOperationCode typeMap codeGenOperation
    CodeGenItemOperationParam codeGenOperationParam ->
      generateOperationParamCode codeGenOperationParam

generateOperationCode ::
  TypeMap ->
  CodeGenOperation ->
  CodeGen (FilePath, HC.HaskellCode)
generateOperationCode _typeMap codeGenOperation = do
  (moduleName, typeName) <-
    inferTypeForInputName
      Operation
      (codeGenOperationOriginalName codeGenOperation)

  beelineMethod <-
    methodToBeelineFunction $
      codeGenOperationMethod codeGenOperation

  let
    filePath =
      T.unpack (T.replace "." "/" (HC.moduleNameToText moduleName) <> ".hs")

    header =
      operationModuleHeader moduleName typeName

    urlPath =
      codeGenOperationPath codeGenOperation

    typeNameAsCode =
      HC.typeNameToCode Nothing typeName

    mkRouteField pathPiece =
      case pathPiece of
        PathParamRef param ->
          Just
            ( HC.toVarName moduleName Nothing (codeGenOperationParamName param)
            , HC.typeNameToCodeDefaultQualification (codeGenOperationParamTypeName param)
            , Nothing
            )
        PathLiteral _ -> Nothing

    routeTypeDeclaration =
      HC.record typeName (mapMaybe mkRouteField urlPath)

    mkPiece pathPiece =
      case pathPiece of
        PathLiteral text -> beelinePiece <> " " <> HC.stringLiteral text
        PathParamRef param ->
          let
            paramName =
              HC.toVarName moduleName Nothing
                . codeGenOperationParamName
                $ param

            paramTypeName =
              codeGenOperationParamTypeName param

            paramDef =
              HC.toVarName
                (HC.typeNameModule paramTypeName)
                (Just . HC.typeNameText $ paramTypeName)
                "def"
          in
            beelineParam
              <> " "
              <> beelineMkParam
              <> " "
              <> HC.varNameToCodeDefaultQualification paramDef
              <> " "
              <> HC.varNameToCode Nothing paramName

    route =
      HC.lines
        ( "route :: " <> beelineRouter <> " r => r " <> typeNameAsCode
            : "route ="
            : HC.indent 2 (beelineMethod <> " " <> HC.dollar)
            : HC.indent 4 (beelineMake <> " " <> typeNameAsCode)
            : fmap (\piece -> HC.indent 6 (mkPiece piece)) urlPath
        )

    moduleBody =
      HC.declarations
        [ routeTypeDeclaration
        , route
        ]

    pragmas =
      HC.lines
        [ "{-# LANGUAGE NoImplicitPrelude #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        ]

    code =
      HC.declarations $
        [ pragmas
        , header
        , importDeclarations moduleName moduleBody
        , moduleBody
        ]

  pure (filePath, code)

operationModuleHeader :: HC.ModuleName -> HC.TypeName -> HC.HaskellCode
operationModuleHeader moduleName typeName =
  HC.lines
    [ "module " <> HC.toCode moduleName
    , HC.indent 2 ("( " <> HC.typeNameToCode Nothing typeName <> "(..)")
    , HC.indent 2 (", route")
    , HC.indent 2 ") where"
    ]

generateOperationParamCode ::
  CodeGenOperationParam ->
  CodeGen (FilePath, HC.HaskellCode)
generateOperationParamCode codeGenOperationParam = do
  let
    paramName =
      codeGenOperationParamName codeGenOperationParam

    typeName =
      codeGenOperationParamTypeName codeGenOperationParam

    moduleName =
      HC.typeNameModule typeName

    filePath =
      T.unpack (T.replace "." "/" (HC.moduleNameToText moduleName) <> ".hs")

    pragmas =
      HC.lines
        [ "{-# LANGUAGE NoImplicitPrelude #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        ]

    header =
      operationParamHeader moduleName typeName

    baseType =
      paramTypeToBaseHaskellType
        (codeGenOperationParamType codeGenOperationParam)

    typeDeclaration =
      HC.newtype_
        typeName
        (HC.fromCode (HC.typeNameToCodeDefaultQualification baseType))

    beelineBaseDef =
      paramTypeToBaseBeelineType
        (codeGenOperationParamType codeGenOperationParam)

    defDeclaration =
      HC.lines
        [ "def :: " <> beelineParamDef <> " " <> HC.typeNameToCode Nothing typeName
        , "def ="
        , HC.indent 2 (beelineCoerceParam <> " (" <> beelineBaseDef <> " " <> HC.stringLiteral paramName <> ")")
        ]

    moduleBody =
      HC.declarations
        [ typeDeclaration
        , defDeclaration
        ]

    code =
      HC.declarations $
        [ pragmas
        , header
        , importDeclarations moduleName moduleBody
        , moduleBody
        ]

  pure (filePath, code)

paramTypeToBaseHaskellType :: OperationParamType -> HC.TypeName
paramTypeToBaseHaskellType paramType =
  case paramType of
    ParamTypeString -> textType
    ParamTypeInteger -> integerType
    ParamTypeInt -> intType
    ParamTypeInt8 -> int8Type
    ParamTypeInt16 -> int16Type
    ParamTypeInt32 -> int32Type
    ParamTypeInt64 -> int64Type
    ParamTypeArray itemType -> paramTypeToBaseHaskellType itemType

paramTypeToBaseBeelineType :: HC.FromCode c => OperationParamType -> c
paramTypeToBaseBeelineType paramType =
  case paramType of
    ParamTypeString -> beelineTextParam
    ParamTypeInteger -> beelineIntegerParam
    ParamTypeInt -> beelineIntParam
    ParamTypeInt8 -> beelineInt8Param
    ParamTypeInt16 -> beelineInt16Param
    ParamTypeInt32 -> beelineInt32Param
    ParamTypeInt64 -> beelineInt64Param
    ParamTypeArray itemType -> paramTypeToBaseBeelineType itemType

operationParamHeader :: HC.ModuleName -> HC.TypeName -> HC.HaskellCode
operationParamHeader moduleName typeName =
  HC.lines
    [ "module " <> HC.toCode moduleName
    , HC.indent 2 ("( " <> HC.typeNameToCode Nothing typeName <> "(..)")
    , HC.indent 2 (", def")
    , HC.indent 2 ") where"
    ]

generateSchemaCode ::
  TypeMap ->
  CodeGenType ->
  CodeGen (FilePath, HC.HaskellCode)
generateSchemaCode typeMap codeGenType = do
  (moduleName, typeName) <-
    inferTypeForInputName Type (codeGenTypeOriginalName codeGenType)

  let
    path =
      T.unpack (T.replace "." "/" (HC.moduleNameToText moduleName) <> ".hs")

    format =
      codeGenTypeDataFormat codeGenType

    header =
      schemaTypeModuleHeader moduleName typeName

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
        , moduleBody
        ]

  pure (path, code)

schemaTypeModuleHeader :: HC.ModuleName -> HC.TypeName -> HC.HaskellCode
schemaTypeModuleHeader moduleName typeName =
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
      HC.newtype_ wrapperName baseType

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
  (_moduleName, typeName) <- inferTypeForInputName Type name

  let
    schema =
      fleeceSchemaNameForType typeName

  pure $
    SchemaTypeInfo
      { schemaTypeExpr = HC.typeNameToCodeDefaultQualification typeName
      , schemaTypeSchema = HC.varNameToCodeDefaultQualification schema
      }

inferTypeForInputName :: CodeSection -> T.Text -> (CodeGen (HC.ModuleName, HC.TypeName))
inferTypeForInputName section inputName = do
  moduleName <- generatedModuleName section inputName
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

data CodeSection
  = Type
  | Operation

sectionModule :: CodeSection -> HC.ModuleName
sectionModule section =
  case section of
    Type -> HC.ModuleName "Types"
    Operation -> HC.ModuleName "Operations"

generatedModuleName :: CodeSection -> T.Text -> CodeGen HC.ModuleName
generatedModuleName section text = do
  options <- ask
  pure $
    HC.ModuleName (moduleBaseName options)
      <> sectionModule section
      <> HC.toModuleName text

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

intType :: HC.TypeName
intType =
  HC.preludeType "Int"

int8Type :: HC.TypeName
int8Type =
  HC.toTypeName "Data.Int" (Just "I") "Int8"

int16Type :: HC.TypeName
int16Type =
  HC.toTypeName "Data.Int" (Just "I") "Int16"

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

methodToBeelineFunction :: HC.FromCode c => T.Text -> CodeGen c
methodToBeelineFunction method =
  fmap beelineRoutingVar $
    case T.toLower method of
      "get" -> pure "get"
      "put" -> pure "put"
      "post" -> pure "post"
      "delete" -> pure "delete"
      "options" -> pure "options"
      "head" -> pure "trace"
      "patch" -> pure "patch"
      "trace" -> pure "trace"
      _ -> codeGenError ("Unsupported operation method: " <> show method)

beelineMake :: HC.FromCode c => c
beelineMake =
  beelineRoutingVar "make"

beelineRouter :: HC.FromCode c => c
beelineRouter =
  beelineRoutingType "Router"

beelinePiece :: (HC.FromCode c, HC.ToCode c) => c
beelinePiece =
  beelineRoutingOperator "/-"

beelineParam :: (HC.FromCode c, HC.ToCode c) => c
beelineParam =
  beelineRoutingOperator "/+"

beelineMkParam :: HC.FromCode c => c
beelineMkParam =
  beelineRoutingConstructor "Param"

beelineParamDef :: HC.FromCode c => c
beelineParamDef =
  beelineRoutingType "ParameterDefinition"

beelineCoerceParam :: HC.FromCode c => c
beelineCoerceParam =
  beelineRoutingVar "coerceParam"

beelineTextParam :: HC.FromCode c => c
beelineTextParam =
  beelineRoutingVar "textParam"

beelineIntegerParam :: HC.FromCode c => c
beelineIntegerParam =
  beelineRoutingVar "integerParam"

beelineIntParam :: HC.FromCode c => c
beelineIntParam =
  beelineRoutingVar "intParam"

beelineInt8Param :: HC.FromCode c => c
beelineInt8Param =
  beelineRoutingVar "int8Param"

beelineInt16Param :: HC.FromCode c => c
beelineInt16Param =
  beelineRoutingVar "int16Param"

beelineInt32Param :: HC.FromCode c => c
beelineInt32Param =
  beelineRoutingVar "int32Param"

beelineInt64Param :: HC.FromCode c => c
beelineInt64Param =
  beelineRoutingVar "int32Param"

beelineRoutingVar :: HC.FromCode c => T.Text -> c
beelineRoutingVar =
  HC.fromCode
    . HC.varNameToCodeDefaultQualification
    . HC.toVarName "Beeline.Routing" (Just "R")

beelineRoutingConstructor :: HC.FromCode c => T.Text -> c
beelineRoutingConstructor =
  HC.fromCode
    . HC.varNameToCodeDefaultQualification
    . HC.toConstructorVarName "Beeline.Routing" (Just "R")

beelineRoutingType :: HC.FromCode c => T.Text -> c
beelineRoutingType =
  HC.fromCode
    . HC.typeNameToCodeDefaultQualification
    . HC.toTypeName "Beeline.Routing" (Just "R")

beelineRoutingOperator :: (HC.FromCode c, HC.ToCode c) => T.Text -> c
beelineRoutingOperator op =
  HC.addReferences
    [HC.VarReference "Beeline.Routing" Nothing ("(" <> op <> ")")]
    (HC.fromText op)
