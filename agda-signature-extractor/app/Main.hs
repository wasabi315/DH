module Main (main) where

import Agda.Compiler.Backend qualified as Agda
import Agda.Interaction.FindFile qualified as Agda
import Agda.Interaction.Imports qualified as Agda
import Agda.Interaction.JSON qualified as Json
import Agda.Interaction.Library qualified as Agda
import Agda.Interaction.Options qualified as Agda
import Agda.Interaction.Options.Warnings qualified as Agda
import Agda.Syntax.Abstract.Name qualified as Abstract
import Agda.Syntax.Common.Pretty qualified as Pretty
import Agda.Syntax.Concrete qualified as Concrete
import Agda.Syntax.Position qualified as Agda
import Agda.Syntax.Translation.AbstractToConcrete qualified as Agda
import Agda.Syntax.Translation.InternalToAbstract qualified as Agda
import Agda.Utils.FileName qualified as Agda
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as ByteString
import Data.Foldable
import Data.List (sort)
import Data.Monoid
import GHC.Generics (Generic)
import Options.Applicative qualified as Opt
import System.Directory
import System.FilePath
import System.FilePath.Find qualified as Find
import System.IO

--------------------------------------------------------------------------------
-- Extract summary information about definitions from Agda library

-- | Load the library file from the given root directory.
loadLibraryFile :: FilePath -> Agda.TCM Agda.AgdaLibFile
loadLibraryFile libraryRootDirectory = do
  libraryFiles <- Agda.libToTCM $ Agda.mkLibM [] $ Agda.getAgdaLibFiles' libraryRootDirectory
  case libraryFiles of
    [libraryFile] -> pure libraryFile
    _ -> throwError $ Agda.Exception Agda.noRange "Expected exactly one library file"

-- | Find the paths to all Agda modules in the given include paths.
findAllModulePath :: [FilePath] -> IO [FilePath]
findAllModulePath includePaths = do
  sort . map Find.infoPath . concat <$> forM includePaths \includePath -> do
    Find.fold
      (pure True)
      ( \foundAgdaFiles foundFile ->
          if Find.evalClause (isExtensionOf ".agda" <$> Find.filePath) foundFile
            then foundFile : foundAgdaFiles
            else foundAgdaFiles
      )
      []
      includePath

-- | Type check the given module and return the interface.
typeCheckModule :: FilePath -> Agda.TCM Agda.Interface
typeCheckModule moduleFilePath = do
  let agdaOptions =
        Agda.defaultOptions
          { Agda.optTraceImports = 0,
            Agda.optPragmaOptions =
              Agda.defaultPragmaOptions
                { Agda._optWarningMode = Agda.defaultWarningMode {Agda._warningSet = Agda.noWarnings}
                }
          }

  moduleAbsolutePath <- liftIO $ Agda.absolute moduleFilePath
  Agda.setCommandLineOptions agdaOptions
  source <- Agda.parseSource (Agda.SourceFile moduleAbsolutePath)
  typeCheckResult <- Agda.typeCheckMain Agda.TypeCheck source
  pure $ Agda.crInterface typeCheckResult

-- | A summary of a definition in the interface.
data DefinitionSummary = DefinitionSummary
  { name :: Abstract.Name,
    topLevelModuleName :: Concrete.QName,
    moduleName :: Concrete.QName,
    qualifiedName :: Concrete.QName,
    kind :: DefinitionKind,
    signature :: Concrete.Expr
  }

data DefinitionKind
  = Axiom
  | Function
  | Datatype
  | Constructor
  | Record
  | Primitive

-- | Convert the given definition to a definition kind.
-- Returns @Nothing@ for uninteresting definitions.
definitionKind :: Agda.Defn -> Maybe DefinitionKind
definitionKind = \case
  Agda.DatatypeDefn {} -> Just Datatype
  Agda.RecordDefn {} -> Just Record
  Agda.ConstructorDefn {} -> Just Constructor
  Agda.AxiomDefn {} -> Just Axiom
  Agda.PrimitiveDefn {} -> Just Primitive
  Agda.PrimitiveSortDefn {} -> Just Primitive
  Agda.FunctionDefn defn
    -- Generated functions are not interesting for us
    | Just _ <- defn._funIsKanOp -> Nothing
    | Just _ <- defn._funWith -> Nothing
    | Just _ <- defn._funExtLam -> Nothing
    | otherwise -> Just Function
  _ -> Nothing

-- | Summarize the given @Agda.Interface@ by extracting the definitions and converting them to @[DefinitionSummary]@.
summarizeInterface :: Agda.Interface -> Agda.TCM [DefinitionSummary]
summarizeInterface interface = do
  let topLevelModuleName = Agda.mnameToConcrete interface.iModuleName
      definitions = interface.iSignature._sigDefinitions

  let Ap action = flip foldMap definitions \definition -> Ap do
        case definitionKind definition.theDef of
          Nothing -> pure mempty
          Just kind -> do
            signature <- Agda.abstractToConcrete_ =<< Agda.reify definition.defType
            let qname = definition.defName
                summary =
                  DefinitionSummary
                    { name = qname.qnameName,
                      topLevelModuleName,
                      moduleName = Agda.mnameToConcrete qname.qnameModule,
                      kind,
                      qualifiedName = Agda.qnameToConcrete qname,
                      signature
                    }
            pure $ Endo (summary :)

  flip appEndo [] <$> action

--------------------------------------------------------------------------------
-- Output the summaries as JSONL documents

-- | A document for a definition summary.
data Document = Document
  { pageContent :: String,
    metadata :: Metadata
  }
  deriving (Generic)

data Metadata = Metadata
  { name :: String,
    topLevelModuleName :: String,
    moduleName :: String,
    qualifiedName :: String,
    kind :: String,
    signature :: String
  }
  deriving (Generic)

instance Json.ToJSON Metadata where
  toEncoding = Json.genericToEncoding Json.defaultOptions {Json.fieldLabelModifier = Json.camelTo2 '_'}

instance Json.ToJSON Document where
  toEncoding = Json.genericToEncoding Json.defaultOptions {Json.fieldLabelModifier = Json.camelTo2 '_'}

-- | Write the given list of values to the given handle as JSONL.
hPutJsonL :: (Json.ToJSON a) => Handle -> [a] -> IO ()
hPutJsonL handle = traverse_ \value -> do
  ByteString.hPutStr handle $ Json.encode value
  ByteString.hPutStr handle "\n"

-- | Render the given document in one line.
renderOneLine :: Pretty.Doc -> String
renderOneLine = Pretty.renderStyle Pretty.style {Pretty.mode = Pretty.OneLineMode}

-- | Convert the given definition summary to a document.
formatDefinitionSummary :: DefinitionSummary -> Document
formatDefinitionSummary summary = Document {..}
  where
    metadata = Metadata {..}
    pageContent = renderOneLine $ Pretty.pretty summary.name <> " : " <> Pretty.pretty summary.signature
    name = renderOneLine $ Pretty.pretty summary.name
    topLevelModuleName = renderOneLine $ Pretty.pretty summary.topLevelModuleName
    moduleName = renderOneLine $ Pretty.pretty summary.moduleName
    qualifiedName = renderOneLine $ Pretty.pretty summary.qualifiedName
    kind = case summary.kind of
      Axiom -> "axiom"
      Function -> "function"
      Datatype -> "datatype"
      Constructor -> "constructor"
      Record -> "record"
      Primitive -> "primitive"
    signature = renderOneLine $ Pretty.pretty summary.signature

--------------------------------------------------------------------------------
-- Command line arguments

data ExtractArguments = ExtractArguments
  { -- | The current working directory of the extractor.
    currentDirectory :: FilePath,
    -- | The relative directory to the Agda library.
    relativeLibraryDirectory :: FilePath,
    -- | The relative directory to write the signatures to.
    relativeOutDirectory :: FilePath
  }

parseArguments :: IO ExtractArguments
parseArguments = do
  currentDirectory <- getCurrentDirectory

  let optionParser =
        ExtractArguments currentDirectory
          <$> Opt.strOption (Opt.long "libDir" <> Opt.metavar "LIB_DIR" <> Opt.help "The *relative* directory to the Agda library. Defaults to the current directory" <> Opt.value "./")
          <*> Opt.strOption (Opt.long "outDir" <> Opt.metavar "OUT_DIR" <> Opt.help "The *relative* directory to write the signatures to")

      parserInfo = Opt.info (optionParser Opt.<**> Opt.helper) (Opt.fullDesc <> Opt.progDesc "Extract signatures from Agda library")

  Opt.execParser parserInfo

libraryDirectory :: ExtractArguments -> FilePath
libraryDirectory arguments =
  arguments.currentDirectory </> arguments.relativeLibraryDirectory

summaryFilePath :: ExtractArguments -> (Abstract.ModuleName -> FilePath)
summaryFilePath arguments = \moduleName ->
  outDirectory
    </> foldr ((</>) . show . Pretty.pretty) "" moduleName.mnameToList
      <.> "jsonl"
  where
    outDirectory = arguments.currentDirectory </> arguments.relativeOutDirectory

--------------------------------------------------------------------------------

withFileEnsuringDirectory :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFileEnsuringDirectory filePath mode action = do
  createDirectoryIfMissing True (takeDirectory filePath)
  withFile filePath mode action

main :: IO ()
main = do
  arguments <- parseArguments

  Agda.runTCMTop' do
    libraryFile <- loadLibraryFile $ libraryDirectory arguments

    Agda.checkAndSetOptionsFromPragma libraryFile._libPragmas

    moduleFilePaths <- liftIO $ findAllModulePath libraryFile._libIncludes

    let getSummaryFilePath = summaryFilePath arguments
    for_ moduleFilePaths \moduleFilePath -> flip catchError (\_ -> pure ()) do
      interface <- typeCheckModule moduleFilePath
      summaries <- summarizeInterface interface
      let path = getSummaryFilePath interface.iModuleName
      liftIO $ withFileEnsuringDirectory path WriteMode \handle ->
        hPutJsonL handle $ map formatDefinitionSummary summaries
