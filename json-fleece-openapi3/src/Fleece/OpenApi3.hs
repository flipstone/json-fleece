{-# LANGUAGE OverloadedStrings #-}

module Fleece.OpenApi3
  ( generateFleeceCode
  , CodeGenOptions
    ( CodeGenOptions
    , moduleBaseName
    )
  , CodeGenResult
  , CodeGenError
    ( UnableToDetermineTypeNameForSchema
    )
  , HC.HaskellCode
  , HC.renderLazyText
  , HC.renderText
  , Modules
  ) where

import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict.InsOrd as IOHM
import qualified Data.Map.Strict as Map
import qualified Data.OpenApi as OA
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified HaskellCode as HC

data CodeGenError
  = UnableToDetermineTypeNameForSchema OA.Schema
  | UnsupportedItem String OA.Schema
  | UnsupportedEnumValue Aeson.Value
  | ReferencedSchemaMissing OA.Reference
  deriving (Show)

data CodeGenContext = CodeGenContext
  { codeGenApi :: OA.OpenApi
  , codeGenOptions :: CodeGenOptions
  }

type CodeGen = ReaderT CodeGenContext (Either CodeGenError)
type CodeGenResult = Either CodeGenError

type Modules =
  [(FilePath, HC.HaskellCode)]

data CodeGenOptions = CodeGenOptions
  { moduleBaseName :: T.Text
  }

codeGenError :: CodeGenError -> CodeGen a
codeGenError = lift . Left

getSchemaDefinitions :: CodeGen (OA.Definitions OA.Schema)
getSchemaDefinitions =
  asks (OA._componentsSchemas . OA._openApiComponents . codeGenApi)

getReferencedSchema :: OA.Referenced OA.Schema -> CodeGen OA.Schema
getReferencedSchema ref =
  case ref of
    OA.Ref reference -> do
      schemas <- getSchemaDefinitions
      case IOHM.lookup (OA.getReference reference) schemas of
        Nothing -> codeGenError (ReferencedSchemaMissing reference)
        Just schema -> pure schema
    OA.Inline schema ->
      pure schema

generateFleeceCode :: CodeGenOptions -> OA.OpenApi -> CodeGenResult Modules
generateFleeceCode options api =
  let
    codeGen :: CodeGen Modules
    codeGen = do
      schemas <- getSchemaDefinitions
      traverse (uncurry generateSchemaCode) (IOHM.toList schemas)

    context =
      CodeGenContext
        { codeGenApi = api
        , codeGenOptions = options
        }
  in
    runReaderT codeGen context

generateSchemaCode :: T.Text -> OA.Schema -> CodeGen (FilePath, HC.HaskellCode)
generateSchemaCode schemaKey schema = do
  moduleName <- generatedModuleName schemaKey

  let
    typeName =
      HC.toTypeName moduleName schemaKey

    path =
      T.unpack (T.replace "." "/" (HC.moduleNameToText moduleName) <> ".hs")

  moduleBody <-
    case OA._schemaType schema of
      Just OA.OpenApiString -> generateHaskellString typeName schema
      Just OA.OpenApiNumber -> pure . HC.toCode $ typeName
      Just OA.OpenApiInteger -> pure . HC.toCode $ typeName
      Just OA.OpenApiBoolean -> pure . HC.toCode $ typeName
      Just OA.OpenApiArray -> pure . HC.toCode $ typeName
      Just OA.OpenApiObject -> generateHaskellObject typeName schema
      Just OA.OpenApiNull -> pure . HC.toCode $ typeName
      Nothing -> codeGenError (UnableToDetermineTypeNameForSchema schema)

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

generateHaskellString :: HC.TypeName -> OA.Schema -> CodeGen HC.HaskellCode
generateHaskellString typeName schema = do
  case OA._schemaEnum schema of
    Just enumValues -> generateHaskellEnum typeName enumValues
    Nothing -> do
      let
        newtypeDecl =
          HC.lines
            [ "newtype " <> HC.toCode typeName <> " Text"
            , HC.indent 2 (HC.deriving_ [HC.showClass, HC.eqClass])
            ]

      fleeceSchema <-
        fleeceSchemaForType typeName $
          [ fleeceCoreVar "coerceSchema" <> " " <> fleeceCoreVar "text"
          ]

      pure $
        HC.declarations
          [ newtypeDecl
          , fleeceSchema
          ]

generateHaskellEnum :: HC.TypeName -> [Aeson.Value] -> CodeGen HC.HaskellCode
generateHaskellEnum typeName enumValues = do
  let
    mkEnumItem value =
      case value of
        Aeson.String t -> pure (t, HC.toConstructorName t)
        _ -> codeGenError (UnsupportedEnumValue value)

  enumItems <- traverse mkEnumItem enumValues
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

generateHaskellObject :: HC.TypeName -> OA.Schema -> CodeGen HC.HaskellCode
generateHaskellObject typeName schema = do
  moduleName <- moduleNameForType typeName

  fields <-
    traverse (uncurry $ mkField moduleName (OA._schemaRequired schema))
      . filter (\(name, _) -> not (name == "_links"))
      . IOHM.toList
      . OA._schemaProperties
      $ schema

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

data Field = Field
  { fieldName :: HC.VarName
  , fieldBaseTypeName :: HC.TypeName
  , fieldJSONName :: OA.ParamName
  , fieldJSONSchema :: OA.Schema
  , fieldRequired :: Bool
  , fieldBaseFleeceSchemaName :: HC.HaskellCode
  , fieldDescription :: Maybe T.Text
  }

fieldTypeName :: Field -> HC.TypeName
fieldTypeName field =
  case (fieldRequired field, fieldNullable field) of
    (True, False) -> fieldBaseTypeName field
    (True, True) ->
      HC.eitherOf
        fleeceCoreNullType
        (fieldBaseTypeName field)
    (False, False) -> HC.maybeOf (fieldBaseTypeName field)
    (False, True) ->
      HC.maybeOf $
        HC.fromCode "("
          <> HC.eitherOf fleeceCoreNullType (fieldBaseTypeName field)
          <> HC.fromCode ")"

fieldNullable :: Field -> Bool
fieldNullable field =
  case OA._schemaNullable (fieldJSONSchema field) of
    Nothing -> False
    Just b -> b

fieldFleeceSchemaCode :: Field -> HC.HaskellCode
fieldFleeceSchemaCode field =
  if fieldNullable field
    then "(" <> fleeceCoreVar "nullable" <> " " <> fieldBaseFleeceSchemaName field <> ")"
    else fieldBaseFleeceSchemaName field

fleeceFieldFunction :: Field -> HC.HaskellCode
fleeceFieldFunction field =
  if fieldRequired field
    then fleeceCoreVar "required"
    else fleeceCoreVar "optional"

mkField ::
  HC.ModuleName ->
  [OA.ParamName] ->
  OA.ParamName ->
  OA.Referenced OA.Schema ->
  CodeGen Field
mkField moduleName requiredParams paramName schemaRef = do
  typeInfo <- schemaRefTypeInfo schemaRef
  fieldSchema <- getReferencedSchema schemaRef

  pure $
    Field
      { fieldName = HC.toVarName moduleName paramName
      , fieldBaseTypeName = schemaTypeName typeInfo
      , fieldRequired = paramName `elem` requiredParams
      , fieldJSONName = paramName
      , fieldJSONSchema = fieldSchema
      , fieldBaseFleeceSchemaName = schemaTypeSchema typeInfo
      , fieldDescription = OA._schemaDescription fieldSchema
      }

data SchemaTypeInfo = SchemaTypeInfo
  { schemaTypeName :: HC.TypeName
  , schemaTypeSchema :: HC.HaskellCode
  }

schemaRefTypeInfo :: OA.Referenced OA.Schema -> CodeGen SchemaTypeInfo
schemaRefTypeInfo schemaRef =
  case schemaRef of
    OA.Ref ref ->
      inferSchemaInfoForOpenApiName . OA.getReference $ ref
    OA.Inline schema ->
      schemaTypeInfo schema

schemaTypeInfo :: OA.Schema -> CodeGen SchemaTypeInfo
schemaTypeInfo schema =
  case OA._schemaTitle schema of
    Just t -> do
      inferSchemaInfoForOpenApiName t
    Nothing ->
      case OA._schemaType schema of
        Just OA.OpenApiString ->
          pure $
            case OA._schemaFormat schema of
              Just "date" ->
                SchemaTypeInfo
                  { schemaTypeName = HC.toTypeName "Data.Time" "Day"
                  , schemaTypeSchema = fleeceCoreVar "day"
                  }
              Just "date-time" ->
                SchemaTypeInfo
                  { schemaTypeName = HC.toTypeName "Data.Time" "UTCTime"
                  , schemaTypeSchema = fleeceCoreVar "utcTime"
                  }
              _ ->
                SchemaTypeInfo
                  { schemaTypeName = textType
                  , schemaTypeSchema = fleeceCoreVar "text"
                  }
        Just OA.OpenApiNumber ->
          pure $
            case OA._schemaFormat schema of
              Just "float" ->
                SchemaTypeInfo
                  { schemaTypeName = HC.toTypeName "Prelude" "Float"
                  , schemaTypeSchema = fleeceCoreVar "float"
                  }
              Just "double" ->
                SchemaTypeInfo
                  { schemaTypeName = HC.toTypeName "Prelude" "Double"
                  , schemaTypeSchema = fleeceCoreVar "double"
                  }
              _ ->
                SchemaTypeInfo
                  { schemaTypeName = HC.toTypeName "Data.Scientific" "Scientific"
                  , schemaTypeSchema = fleeceCoreVar "number"
                  }
        Just OA.OpenApiInteger ->
          pure $
            case OA._schemaFormat schema of
              Just "int32" ->
                SchemaTypeInfo
                  { schemaTypeName = HC.toTypeName "Data.Int" "Int32"
                  , schemaTypeSchema = fleeceCoreVar "int32"
                  }
              Just "int64" ->
                SchemaTypeInfo
                  { schemaTypeName = HC.toTypeName "Data.Int" "Int64"
                  , schemaTypeSchema = fleeceCoreVar "int64"
                  }
              _ ->
                SchemaTypeInfo
                  { schemaTypeName = HC.toTypeName "Prelude" "Integer"
                  , schemaTypeSchema = fleeceCoreVar "integer"
                  }
        Just OA.OpenApiBoolean ->
          pure $
            SchemaTypeInfo
              { schemaTypeName = HC.toTypeName "Prelude" "Bool"
              , schemaTypeSchema = fleeceCoreVar "boolean"
              }
        Just OA.OpenApiArray ->
          case OA._schemaItems schema of
            Nothing -> codeGenError (UnableToDetermineTypeNameForSchema schema)
            Just (OA.OpenApiItemsObject itemSchemaRef) -> do
              itemInfo <- schemaRefTypeInfo itemSchemaRef

              pure $
                SchemaTypeInfo
                  { schemaTypeName = HC.listOf (schemaTypeName itemInfo)
                  , schemaTypeSchema =
                      "("
                        <> fleeceCoreVar "list"
                        <> " "
                        <> schemaTypeSchema itemInfo
                        <> ")"
                  }
            Just (OA.OpenApiItemsArray _itemSchemaRefs) ->
              codeGenError (UnsupportedItem "Heterogenous Array" schema)
        Just OA.OpenApiObject -> codeGenError (UnsupportedItem "Object" schema)
        Just OA.OpenApiNull -> codeGenError (UnsupportedItem "Null" schema)
        Nothing -> codeGenError (UnableToDetermineTypeNameForSchema schema)

inferSchemaInfoForOpenApiName :: T.Text -> CodeGen SchemaTypeInfo
inferSchemaInfoForOpenApiName name = do
  moduleName <- generatedModuleName name

  let
    typeName =
      HC.toTypeName moduleName name

  schema <- fleeceSchemaNameForType typeName
  pure $
    SchemaTypeInfo
      { schemaTypeName = typeName
      , schemaTypeSchema = HC.toCode schema
      }

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
moduleNameForType = do
  generatedModuleName . HC.renderText

generatedModuleName :: T.Text -> CodeGen HC.ModuleName
generatedModuleName text = do
  options <- asks codeGenOptions
  pure $
    HC.ModuleName (moduleBaseName options) <> HC.toModuleName text

textType :: HC.TypeName
textType =
  HC.toTypeName "Data.Text" "Text"

fleeceCoreNullType :: HC.TypeName
fleeceCoreNullType =
  fleeceCoreType "Null"

fleeceClass :: HC.TypeName
fleeceClass =
  fleeceCoreType "Fleece"

fleeceCoreType :: T.Text -> HC.TypeName
fleeceCoreType =
  HC.toQualifiedTypeName "Fleece.Core" "FC"

fleeceCoreVar :: HC.FromCode c => T.Text -> c
fleeceCoreVar =
  HC.fromCode . HC.toCode . HC.toQualifiedVarName "Fleece.Core" "FC"
