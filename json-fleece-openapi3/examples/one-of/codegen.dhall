let
  CodeGen =
    env:CODEGEN_TEST_PRELUDE
    ? ../../../json-fleece-codegen-util/codegen-prelude.dhall
in
  \(rootDir : Text) ->
    CodeGen.baseConfig //
      { moduleBaseName = "OneOfTest"
      , inputFileName = "${rootDir}/one-of.yaml"
      , destination = rootDir
      }
