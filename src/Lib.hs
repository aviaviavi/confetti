{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}

module Lib where

import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.List.Utils
import           Data.Maybe
import           Data.Time.Clock.POSIX
import           Data.Yaml
import           GHC.Generics
import           System.Directory
import           System.FilePath.Posix
import           System.IO.Error
import           System.Posix.Files
import           Text.Printf

import qualified Data.Text             as T

-- Errors from parsing or validating .confetti.yml
data ParseError a = ConfettiYamlNotFound | GroupNotFound a | ConfettiYamlInvalid a | DuplicateNameError a deriving (Generic)

instance (Show a) =>
         Show (ParseError a) where
  show (ConfettiYamlInvalid a) =
    "There was an issue parsing your ~/.confetti.yml: " ++ show a
  show (GroupNotFound a) =
    printf "No match for group %s found in your ~/.confetti.yml: " (show a)
  show ConfettiYamlNotFound =
    "No confetti spec file found! See https://github.com/aviaviavi/confetti if you need help setting one up"
  show (DuplicateNameError a) =
    printf
      "Multiple targets in this group share the same name: %s.\
       \ Confetti doesn't yet know how to figure out target to link a search match to,\
       \ but a future version will. Try breaking these targets into multiple groups."
      (show a)

-- Errors from applying our config spec
newtype ApplyError a = VariantsMissing [a]
instance (Show a) => Show (ApplyError a)  where
  show (VariantsMissing a) = "Couldn't find one or more of your variant files to use: " ++ show a

-- A config file version we want to swap in or out
type ConfigVariant = String

-- The config file version we want to swap in or out, with no directory
-- information, eg `credentials`, not `~/.aws/credentials`
type ConfigVariantFileName = String

-- The prefix specifies how we construct a variant.
-- If the target is config.json, and we want to link local.config.json,
-- (Just "local") would be the prefix
type ConfigVariantPrefix = Maybe String

-- Small helper for printing prefix
showPrefix :: ConfigVariantPrefix -> String
showPrefix prefix = case prefix of
  (Just p) -> show p
  Nothing  -> "bare matches in search paths"

-- The actual config file in question.
type ConfigTarget = String

-- Lets us specify paths where the variant files in a group can be found.
data SearchPath = SearchPath
  { path      :: FilePath
  , recursive :: Maybe Bool
  } deriving (Show, Generic)

-- Represents a search result for a given target, variant and directory
data VariantSearch = VariantSearch
  { searchDirectory :: FilePath
  , fileName        :: ConfigVariantFileName
  , recursiveSearch :: Bool
  , result          :: Maybe ConfigVariant -- populated when find the file we want to swap in
  , linkToCreate :: ConfigTarget
  } deriving (Show, Generic)

-- A set of configuration targets
data ConfigGroup = ConfigGroup
  { name        :: T.Text
  , targets     :: [FilePath]
  , searchPaths :: Maybe [SearchPath]
  } deriving (Show, Generic)

instance FromJSON ConfigGroup where
  parseJSON (Object x) = ConfigGroup <$> x .: "name" <*> x .: "targets" <*> x .:? "search_paths"
  parseJSON _ = fail "Expected an object"

-- A valid .confetti.yml gets parsed into this structure
newtype ParsedSpecFile = ParsedSpecFile
  { groups :: [ConfigGroup]
  } deriving (Show, Generic)

instance ToJSON ConfigGroup
instance ToJSON ParsedSpecFile
instance ToJSON SearchPath
instance FromJSON ParsedSpecFile
instance FromJSON SearchPath

-- A full config specification:
-- a group of files, and a variant to swap in for
-- every target in the group
data ConfigSpec = ConfigSpec
  { configGroup         :: ConfigGroup
  , configVariantPrefix :: ConfigVariantPrefix
  }

-- Given a yaml file and a group name, parse the group into a ConfigGroup, or
-- a ParseError
parseGroup :: FilePath -> T.Text -> IO (Either (ParseError T.Text) ConfigGroup)
parseGroup specFile groupName =
  doesFileExist specFile >>= \exists -> parseGroup' exists
  where
    parseGroup' exists
      | not exists = return $ Left ConfettiYamlNotFound
      | otherwise = do
        eitherParsed <- decodeFileEither specFile
        either
          (return .
           Left . ConfettiYamlInvalid . T.pack . prettyPrintParseException)
          (`findGroup` groupName)
          eitherParsed

-- Any custom validation we want to do on a config group we've successfully parsed
-- goes here
validateSpec :: ConfigGroup -> Either (ParseError T.Text) ConfigGroup
validateSpec groupSpec = let targetFileNames = map takeFileName $ targets groupSpec in
  if length targetFileNames > length (nub targetFileNames)
  then Left (DuplicateNameError $ T.pack . head $ targetFileNames \\ nub targetFileNames)
  else Right groupSpec

-- Finds a given group in a parsed spec file. Returns a GroupNotFound if the group
-- is missing
findGroup :: ParsedSpecFile -> T.Text -> IO  (Either (ParseError T.Text) ConfigGroup)
findGroup spec groupName =
  sequence $
  maybe (Left $ GroupNotFound groupName) (Right . expandPathsForGroup) $
  find (\g -> name g == groupName) (groups spec)

-- Replaces ~ with the value of $HOME for all files in the group
expandPathsForGroup :: ConfigGroup -> IO ConfigGroup
expandPathsForGroup confGroup =
  mapM absolutePath (targets confGroup) >>= \expanded ->
    return $ confGroup {targets = expanded}

-- If a target config file is _not_ a symlink,
-- make a backup before we swap out the config
backUpIfNonSymLink :: FilePath -> IO ()
backUpIfNonSymLink file = do
  exists <- doesFileExist file
  isLink <- if exists then pathIsSymbolicLink file else return False
  when (exists && not isLink) $ createBackup file

-- Backs up a file, eg config.json -> config.json.$time.backup
createBackup :: FilePath -> IO ()
createBackup file =
  let newName =
        (round <$> getPOSIXTime) >>=
        (\t -> return $ file ++ "." ++ show t ++ ".backup")
  in newName >>= \backup -> copyFile file backup

removeIfExists :: FilePath -> IO ()
removeIfExists f = removeFile f `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

-- Given a list of variant configs, returns the variants that
-- do not exist
filterMissingVariants :: [ConfigVariant] -> IO [FilePath]
filterMissingVariants = filterM (fmap not . doesFileExist)

-- This is where the actual config swapping happens. For every ConfigTarget,
-- creates a symlink from the target -> variant.
-- For instance, for target = ~/.aws/credentials, and variant "work",
-- the link created would be
-- ~/.aws/credentials -> ~/.aws/work.credentials
linkTargets :: [VariantSearch] -> IO ()
linkTargets =
  mapM_
    (\pair -> createSymbolicLink (fromJust $ result pair) (linkToCreate pair))

-- Given a variant prefix, ie `dev`, and a target, ie `~/.aws/credentials`,
-- construct the full path of the variant, `~/.aws/dev.credentials`
makeVariant :: ConfigVariantPrefix -> ConfigTarget -> ConfigVariantFileName
makeVariant prefix target = let p = maybe "" (++ ".") prefix in
  p ++ takeFileName target

-- Given a prefix, a list of targets, construct a list of variant filenames, and search
-- for matches in all of the given search paths
searchVariants :: ConfigVariantPrefix -> [ConfigTarget] -> [SearchPath] -> IO [VariantSearch]
searchVariants variant targetFiles vPaths =
  concat <$>
  mapM
    (\target -> mapM (findVariantInPath variant target) vPaths)
    targetFiles

-- Get all the contents of a directory recursively
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \n -> do
    let nextPath = topdir </> n
    isDir <- doesDirectoryExist nextPath
    if isDir
      then getRecursiveContents nextPath
      else return [nextPath]
  return (concat paths)

-- Perform a search for a single file, and return the search result
findVariantInPath :: ConfigVariantPrefix
                  -> ConfigTarget
                  -> SearchPath
                  -> IO VariantSearch
findVariantInPath prefix target searchPath =
  let fileToFind = makeVariant prefix target
  in do
        pathName <- absolutePath $ path searchPath
        if fromMaybe False $ recursive searchPath
          then do
            searchResult <- find (\f -> endswith fileToFind f && (target /= f)) <$> getRecursiveContents pathName
            return
                      VariantSearch
                      { searchDirectory = path searchPath
                      , fileName = fileToFind
                      , recursiveSearch = False
                      , result = searchResult
                      , linkToCreate = target
                      }
          else do
            let potentialVariant = pathName </> fileToFind
            exists <- doesFileExist potentialVariant
            return
                      VariantSearch
                      { searchDirectory = path searchPath
                      , fileName = fileToFind
                      , recursiveSearch = False
                      , linkToCreate = target
                      , result =
                          if exists && potentialVariant /= target
                            then Just potentialVariant
                            else Nothing
                      }


-- Run the spec! Every target will be a symlink to the variant
-- file
applySpec :: ConfigSpec -> IO (Maybe (ApplyError FilePath))
applySpec spec = do
  let groupTargets = targets $ configGroup spec
  searchResults <-
    searchVariants
      (configVariantPrefix spec)
      groupTargets
      (fromMaybe
         (map
            (\t -> SearchPath {path = takeDirectory t, recursive = Just False})
            groupTargets)
         (searchPaths $ configGroup spec))
  mapM_ backUpIfNonSymLink groupTargets
  mapM_ removeIfExists groupTargets
  let confirmedVariantFiles = filter (isJust . result) searchResults
      foundFiles = uniq $ map (takeFileName . fromJust . result) confirmedVariantFiles
      allFiles = uniq $ map fileName searchResults
      missingVariants = allFiles \\ foundFiles
  if null missingVariants
    then do
      mapM_
        (\s -> printSuccess $ linkToCreate s ++ " -> " ++ fromJust (result s))
        confirmedVariantFiles
      linkTargets confirmedVariantFiles
      return Nothing
    else return $ Just (VariantsMissing missingVariants)

-- Expands ~ to $HOME in a path
absolutePath :: FilePath -> IO FilePath
absolutePath p = do
  home <- getHomeDirectory
  return $ replace "~" home p

-- Prints green
printSuccess :: String -> IO ()
printSuccess s = putStrLn $ "\x1b[32m" ++  s

-- Prints red
printFail :: String -> IO ()
printFail s = putStrLn $ "\x1b[31m" ++  s
