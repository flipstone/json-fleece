let
  CodeGen =
    env:CODEGEN_TEST_PRELUDE
    ? ../../../json-fleece-codegen-util/codegen-prelude.dhall
in
  \(rootDir : Text) ->
    CodeGen.baseConfig //
      { moduleBaseName = "SelectedItemsExample"
      , inputFileName = "${rootDir}/selected-items-example.yaml"
      , destination = rootDir
      , selectedItems = CodeGen.selectedItems
          [ CodeGen.operationId "listPets"
          , CodeGen.operationId "createPet"
          , CodeGen.operationId "getInventory"
          , CodeGen.pathMethod "/health" "GET"
          , CodeGen.component "Habitat"
          ]
      }
