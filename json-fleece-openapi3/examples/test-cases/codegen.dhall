let
  CodeGen =
    env:CODEGEN_TEST_PRELUDE
    ? ../../../json-fleece-codegen-util/codegen-prelude.dhall
in
  \(rootDir : Text) ->
    CodeGen.baseConfig //
      { moduleBaseName = "TestCases"
      , inputFileName = "${rootDir}/test-cases.yaml"
      , destination = rootDir
      , typeOptions =
          [ { type = "TestCases.Types.DateTimeFormats.DateTimeFormats"
            , options =
                CodeGen.TypeOptions::
                  { deriveClasses = CodeGen.derive [ CodeGen.show ]
                  }
            }
          , { type = "TestCases.Types.DateTimeFormats.UtcTimeField.UtcTimeField"
            , options =
                CodeGen.TypeOptions::
                  { dateTimeFormat = CodeGen.utcTime
                  }
            }
          , { type = "TestCases.Types.DateTimeFormats.ZonedTimeField.ZonedTimeField"
            , options =
                CodeGen.TypeOptions::
                  { dateTimeFormat = CodeGen.zonedTime
                  , deriveClasses = CodeGen.derive [ CodeGen.show ]
                  }
            }
          , { type = "TestCases.Types.DateTimeFormats.LocalTimeField.LocalTimeField"
            , options =
                CodeGen.TypeOptions::
                  { dateTimeFormat = CodeGen.localTime
                  }
            }
          , { type = "TestCases.Types.UtcTimeType.UtcTimeType"
            , options =
                CodeGen.TypeOptions::
                  { dateTimeFormat = CodeGen.utcTime
                  }
            }
          , { type = "TestCases.Types.ZonedTimeType.ZonedTimeType"
            , options =
                CodeGen.TypeOptions::
                  { dateTimeFormat = CodeGen.zonedTime
                  , deriveClasses = CodeGen.derive [ CodeGen.show ]
                  }
            }
          , { type = "TestCases.Types.LocalTimeType.LocalTimeType"
            , options =
                CodeGen.TypeOptions::
                  { dateTimeFormat = CodeGen.localTime
                  }
            }
          , { type = "TestCases.Types.DerivingNothing.DerivingNothing"
            , options =
                CodeGen.TypeOptions::
                  { deriveClasses = CodeGen.derive CodeGen.noClasses
                  }
            }
          , { type = "TestCases.Operations.TestCases.OperationTypeOptions.PathParam.PathParams"
            , options =
                CodeGen.TypeOptions::
                  { deriveClasses = CodeGen.derive CodeGen.noClasses
                  }
            }
          , { type = "TestCases.Operations.TestCases.OperationTypeOptions.PathParam.QueryParams"
            , options =
                CodeGen.TypeOptions::
                  { deriveClasses = CodeGen.derive CodeGen.noClasses
                  }
            }
          , { type = "TestCases.Operations.TestCases.OperationTypeOptions.PathParam.Responses"
            , options =
                CodeGen.TypeOptions::
                  { deriveClasses = CodeGen.derive CodeGen.noClasses
                  }
            }
          ]
      }
