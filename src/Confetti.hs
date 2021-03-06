{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Confetti where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Either.Utils
import           Data.List
import           Data.List.Utils
import           Data.Maybe
import           Data.Monoid
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
data ApplyError a = VariantsMissing [a] | VariantAlreadyExists [a] deriving (Foldable)
instance (Show a) => Show (ApplyError a)  where
  show (VariantsMissing a) = "Couldn't find one or more of your variant files to use: " ++ show a
  show (VariantAlreadyExists a) = printf "Target(s) %s already exists as a regular file. To backup and then symlink, use the -f flag when invoking confetti" $ show a

appendVariantExists :: ApplyError a -> ApplyError a -> ApplyError a
appendVariantExists (VariantAlreadyExists a) (VariantAlreadyExists b) =
  VariantAlreadyExists $ a ++ b

concatVariantExists :: [ApplyError a] -> ApplyError a
concatVariantExists = foldr appendVariantExists (VariantAlreadyExists [])

maybeApplyError :: ApplyError a -> Maybe (ApplyError a)
maybeApplyError err = if null err then Nothing else Just err

-- applyErrorToMaybe :: ApplyError a -> Maybe (ApplyError a)
--   applyErrorToMaybe a = fmap listToMaybe

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
  } deriving (Show, Eq, Generic)

instance FromJSON SearchPath

-- Represents a search result for a given target, variant and directory
data VariantSearch = VariantSearch
  { searchDirectory :: FilePath
  , fileName        :: ConfigVariantFileName
  , recursiveSearch :: Bool
  , result          :: Maybe ConfigVariant -- populated when find the file we want to swap in
  , linkToCreate    :: ConfigTarget
  } deriving (Show, Generic)

-- A set of configuration targets
data ConfigGroup = ConfigGroup
  { name        :: T.Text
  , targets     :: [FilePath]
  , searchPaths :: Maybe [SearchPath]
  } deriving (Show, Generic)

instance FromJSON ConfigGroup where
  parseJSON (Object x) =
    ConfigGroup <$> x .: "name" <*> x .: "targets" <*> x .:? "search_paths"
  parseJSON _ = fail "Expected an object"

data CommonConfigGroup = CommonConfigGroup
  { commonTargets     :: [FilePath]
  , commonSearchPaths :: Maybe [SearchPath]
  } deriving (Show, Generic)

instance FromJSON CommonConfigGroup where
  parseJSON (Object x) =
    CommonConfigGroup <$> x .: "targets" <*> x .:? "search_paths"
  parseJSON _ = fail "Expected an object"

-- A valid .confetti.yml gets parsed into this structure
data ParsedSpecFile = ParsedSpecFile
  { groups      :: [ConfigGroup]
  , commonGroup :: Maybe CommonConfigGroup
  } deriving (Show, Generic)

instance FromJSON ParsedSpecFile where
  parseJSON (Object x) =
    ParsedSpecFile <$> x .: "groups" <*> x .:? "common"
  parseJSON _ = fail "Expected an object"

-- A full config specification:
-- a group of files, and a variant to swap in for
-- every target in the group
data ConfigSpec = ConfigSpec
  { configGroup         :: ConfigGroup
  , configVariantPrefix :: ConfigVariantPrefix
  , forceSymlink        :: Bool
  }

-- Given a yaml file and a group name, parse the group into a ConfigGroup, or
-- a ParseError.
-- If a `common` group is specified, that group will be combined with the parsed one
parseGroup :: FilePath -> T.Text -> IO (Either (ParseError T.Text) ConfigGroup)
parseGroup specFile groupName =
  doesFileExist specFile >>= \exists -> parseGroup' exists
  where
    parseGroup' exists
      | not exists = return $ Left ConfettiYamlNotFound
      | otherwise = do
        eitherSpec <- decodeFileEither specFile
        eitherGroup <-
          either
            (return .
             Left . ConfettiYamlInvalid . T.pack . prettyPrintParseException)
            (`findGroup` groupName)
            eitherSpec
        either
          (return . Left)
          (\g ->
             let spec = fromRight eitherSpec
             in if isJust $ commonGroup spec
                  then Right <$>
                       appendCommonGroup g (fromJust $ commonGroup spec)
                  else return $ Right g)
          eitherGroup

-- Combine whatever group we parsed with the common group, if one was
-- specified
appendCommonGroup :: ConfigGroup -> CommonConfigGroup -> IO ConfigGroup
appendCommonGroup g common = do
  cTargets <- mapM absolutePath $ commonTargets common
  cSearchPaths <- defaultSearchPaths cTargets (commonSearchPaths common)
  adjustedGroupSearchPaths <- defaultSearchPaths (targets g) (searchPaths g)
  return
    ConfigGroup
    { name = name g
    , targets = uniq $ targets g ++ cTargets
    , searchPaths = Just . uniq $ adjustedGroupSearchPaths <> cSearchPaths
    }

-- Expand all search paths. If search paths are empty, use the paths of the
-- supplied targets
defaultSearchPaths :: [ConfigTarget] -> Maybe [SearchPath] -> IO [SearchPath]
defaultSearchPaths ts ss =
  let unadjusted =
        fromMaybe
           (map
              (\t -> SearchPath {path = takeDirectory t, recursive = Just False})
              ts)
           ss
  in
  mapM
    (\s -> do
       absolute <- absolutePath (path s)
       return $ s {path = absolute})
    unadjusted

-- Any custom validation we want to do on a config group we've successfully parsed
-- goes here
validateSpec :: ConfigGroup -> Either (ParseError T.Text) ConfigGroup
validateSpec groupSpec =
  let targetFileNames = map takeFileName $ targets groupSpec
  in if length targetFileNames > length (uniq targetFileNames)
       then Left
              (DuplicateNameError $
               T.pack . head $ targetFileNames \\ uniq targetFileNames)
       else Right groupSpec

-- Finds a given group in a parsed spec file. Returns a GroupNotFound if the group
-- is missing
findGroup :: ParsedSpecFile
          -> T.Text
          -> IO (Either (ParseError T.Text) ConfigGroup)
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
backUpIfNonSymLink :: Bool -> FilePath -> IO (Maybe (ApplyError FilePath))
backUpIfNonSymLink shouldForce file = do
  exists <- doesFileExist file
  isLink <-
    if exists
      then pathIsSymbolicLink file
      else return False
  if exists && not isLink then
    if shouldForce then createBackup file >> return Nothing
    else return . Just $ VariantAlreadyExists [file]
  else return Nothing

-- Backs up a file, eg config.json -> config.json.$time.backup
createBackup :: FilePath -> IO ()
createBackup file =
  let newName =
        (round <$> getPOSIXTime) >>=
        (\t -> return $ file ++ "." ++ show t ++ ".backup")
  in newName >>= \backup -> copyFile file backup

removeIfExists :: FilePath -> IO ()
removeIfExists f = removeFile f `catch` handleExists
  where
    handleExists e
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

-- Given a variant prefix, eg `dev`, and a target, eg `~/.aws/credentials`,
-- construct the full path of the variant, `~/.aws/dev.credentials`
makeVariant :: ConfigVariantPrefix -> ConfigTarget -> ConfigVariantFileName
makeVariant prefix target =
  let p = maybe "" (++ ".") prefix
   in T.unpack $ T.replace ".." "." (T.pack $  p ++ takeFileName target)

-- Given a prefix, a list of targets, construct a list of variant filenames, and search
-- for matches in all of the given search paths
searchVariants :: ConfigVariantPrefix
               -> [ConfigTarget]
               -> [SearchPath]
               -> IO [VariantSearch]
searchVariants variant targetFiles vPaths =
  concat <$>
  mapM (\target -> mapM (findVariantInPath variant target) vPaths) targetFiles

-- Get all the contents of a directory recursively
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <-
    forM properNames $ \n -> do
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
  in do pathName <- absolutePath $ path searchPath
        let isRecursive = fromMaybe False $ recursive searchPath
        searchResult <-
          if isRecursive
            then find (\f -> endswith fileToFind f && (target /= f)) <$>
                 getRecursiveContents pathName
            else do
              let potentialVariant = pathName </> fileToFind
              exists <- doesFileExist potentialVariant
              if exists
                then return $ Just potentialVariant
                else return Nothing
        return
          VariantSearch
          { searchDirectory = path searchPath
          , fileName = fileToFind
          , recursiveSearch = isRecursive
          , linkToCreate = target
          , result = searchResult
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
         (nub $ map
            (\t -> SearchPath {path = takeDirectory t, recursive = Just False})
            groupTargets)
         (searchPaths $ configGroup spec))
  backupErr <- maybeApplyError . concatVariantExists . catMaybes <$> mapM (backUpIfNonSymLink (forceSymlink spec)) groupTargets
  if isJust backupErr
    then return backupErr
    else do
      let confirmedVariantFiles = filter (isJust . result) searchResults
          foundFiles =
            uniq $ map (takeFileName . fromJust . result) confirmedVariantFiles
          allFiles = uniq $ map fileName searchResults
          missingVariants = allFiles \\ foundFiles
      if null missingVariants
        then do
          mapM_ removeIfExists groupTargets
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
printSuccess s = putStrLn $ "\x1b[32m" ++ s ++ "\x1B[0m"

-- Prints red
printFail :: String -> IO ()
printFail s = putStrLn $ "\x1b[31m" ++  s ++ "\x1B[0m"
