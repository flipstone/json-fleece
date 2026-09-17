let
  CodeGen =
    env:CODEGEN_TEST_PRELUDE
    ? ../../../json-fleece-codegen-util/codegen-prelude.dhall
in
  \(rootDir : Text) ->
    CodeGen.baseConfig //
      { moduleBaseName = "Guard"
      , inputFileName = "${rootDir}/path-ref-filtered.yaml"
      , destination = rootDir
      , selectedItems = CodeGen.selectedItems [ CodeGen.operationId "getThings" ]
      }
