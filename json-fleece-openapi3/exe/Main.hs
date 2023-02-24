module Main
  ( main
  ) where

import qualified Data.Char as Char
import Data.Foldable (traverse_)
import qualified Data.OpenApi as OA
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Yaml.Aeson as YA
import qualified Options.Applicative as Opt
import System.Console.Isocline (readline)
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import System.FilePath (takeDirectory, (</>))

import qualified Fleece.OpenApi3 as FOA3

data Options = Options
  { moduleBaseName :: T.Text
  , fileName :: FilePath
  , mode :: Mode
  , destination :: FilePath
  , noConfirm :: Bool
  }

data Mode
  = Preview
  | Run

optionsInfo :: Opt.ParserInfo Options
optionsInfo =
  Opt.info
    ( Options
        <$> Opt.strArgument (Opt.metavar "MODULE_BASE_NAME")
        <*> Opt.strArgument (Opt.metavar "OPEN_API_3.yml")
        <*> modeParser
        <*> destinationParser
        <*> Opt.flag False True (Opt.long "yes")
    )
    mempty

modeParser :: Opt.Parser Mode
modeParser =
  Opt.flag
    Run
    Preview
    (Opt.long "preview" <> Opt.short 'p')

destinationParser :: Opt.Parser FilePath
destinationParser =
  Opt.strOption
    ( Opt.long "dest"
        <> Opt.short 'd'
        <> Opt.metavar "DESTINATION"
        <> Opt.value "."
    )

main :: IO ()
main = do
  options <- Opt.customExecParser parserPrefs optionsInfo
  openApi <- loadOpenApiOrDie (fileName options)

  let
    codeGenOptions =
      FOA3.CodeGenOptions
        { FOA3.moduleBaseName = moduleBaseName options
        }

  case FOA3.generateFleeceCode codeGenOptions openApi of
    Left err -> Exit.die (show err)
    Right modules ->
      case mode options of
        Preview -> traverse_ (uncurry previewModule) modules
        Run -> createFiles options modules

previewModule :: FilePath -> FOA3.HaskellCode -> IO ()
previewModule path code = do
  putStrLn "===="
  putStrLn path
  putStrLn "===="
  LTIO.putStrLn (FOA3.renderLazyText code)

parserPrefs :: Opt.ParserPrefs
parserPrefs =
  Opt.prefs $
    Opt.showHelpOnEmpty
      <> Opt.showHelpOnError

loadOpenApiOrDie :: FilePath -> IO OA.OpenApi
loadOpenApiOrDie path = do
  errOrOpenApi <- YA.decodeFileEither path
  case errOrOpenApi of
    Left err -> Exit.die (show err)
    Right openApi -> pure openApi

createFiles :: Options -> [(FilePath, FOA3.HaskellCode)] -> IO ()
createFiles options modules = do
  confirm options modules
  traverse_ (uncurry $ createFile options) modules

createFile :: Options -> FilePath -> FOA3.HaskellCode -> IO ()
createFile options path code = do
  let
    fullPath =
      destination options </> path

    directory =
      takeDirectory fullPath

  Dir.createDirectoryIfMissing True directory
  LTIO.writeFile fullPath (FOA3.renderLazyText code)

confirm :: Options -> [(FilePath, FOA3.HaskellCode)] -> IO ()
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
