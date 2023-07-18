{-# LANGUAGE OverloadedStrings #-}

module Fleece.CodeGenUtil.Executable
  ( codeGenMain
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Yaml.Aeson as YA
import qualified Dhall
import qualified Options.Applicative as Opt
import System.Console.Isocline (readline)
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import System.FilePath (takeDirectory, takeExtension, (</>))

import qualified Fleece.CodeGenUtil as CGU
import qualified Fleece.CodeGenUtil.Config as Config

data Options = Options
  { configFileName :: FilePath
  , mode :: Mode
  , noConfirm :: Bool
  }

data Mode
  = Preview
  | Run

optionsInfo :: Opt.ParserInfo Options
optionsInfo =
  Opt.info
    ( Options
        <$> Opt.strArgument (Opt.metavar "codegen.dhall")
        <*> modeParser
        <*> Opt.flag False True (Opt.long "yes")
    )
    mempty

modeParser :: Opt.Parser Mode
modeParser =
  Opt.flag
    Run
    Preview
    (Opt.long "preview" <> Opt.short 'p')

codeGenMain :: (Aeson.FromJSON a) => (a -> CGU.CodeGen CGU.Modules) -> IO ()
codeGenMain generateModules = do
  options <- Opt.customExecParser parserPrefs optionsInfo
  mkConfig <- Dhall.inputFile Config.decoder (configFileName options)

  let
    rootDir =
      takeDirectory (configFileName options)

    config =
      mkConfig (T.pack rootDir)

  source <- loadSourceOrDie (Config.inputFileName config)

  case CGU.runCodeGen (Config.codeGenOptions config) (generateModules source) of
    Left err -> Exit.die (show err)
    Right modules ->
      case mode options of
        Preview -> traverse_ (uncurry previewModule) modules
        Run -> createFiles options config modules

previewModule :: FilePath -> CGU.HaskellCode -> IO ()
previewModule path code = do
  putStrLn "===="
  putStrLn path
  putStrLn "===="
  LTIO.putStrLn (CGU.renderLazyText code)

parserPrefs :: Opt.ParserPrefs
parserPrefs =
  Opt.prefs $
    Opt.showHelpOnEmpty
      <> Opt.showHelpOnError

loadSourceOrDie :: (Aeson.FromJSON a) => FilePath -> IO a
loadSourceOrDie path = do
  let
    showYamlError =
      fmap (either (Left . show) Right)

  errOrSource <-
    case map Char.toLower (takeExtension path) of
      ".yaml" -> showYamlError $ YA.decodeFileEither path
      ".yml" -> showYamlError $ YA.decodeFileEither path
      ".json" -> Aeson.eitherDecodeFileStrict path
      ext -> pure . Left $ "Unsupported file format: " <> show ext

  case errOrSource of
    Left err -> Exit.die (show err)
    Right source -> pure source

createFiles :: Options -> Config.Config -> [(FilePath, CGU.HaskellCode)] -> IO ()
createFiles options config modules = do
  confirm options modules
  traverse_ (uncurry $ createFile config) modules

createFile :: Config.Config -> FilePath -> CGU.HaskellCode -> IO ()
createFile config path code = do
  let
    fullPath =
      Config.destination config </> path

    directory =
      takeDirectory fullPath

  Dir.createDirectoryIfMissing True directory
  LTIO.writeFile fullPath (CGU.renderLazyText code)

confirm :: Options -> [(FilePath, CGU.HaskellCode)] -> IO ()
confirm options modules =
  if noConfirm options
    then pure ()
    else do
      let
        go = do
          answer <- readline "Is this what you want? (yes/no)"

          case map Char.toLower answer of
            "yes" -> pure ()
            "no" -> Exit.die "Aborting at user request"
            _ -> go

      putStrLn $
        "This will create "
          <> show (length modules)
          <> " total files in the destination directory."

      go
