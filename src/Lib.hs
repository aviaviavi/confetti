{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.List.Utils
import           Data.Time.Clock.POSIX
import           Data.Yaml
import           GHC.Generics
import           System.Directory
import           System.IO.Error
import           System.Posix.Files
import           Text.Printf

import qualified Data.Text             as T

-- Errors from parsing .confetti.yml
data ParseError a = ConfettiYamlNotFound | GroupNotFound a | ConfettiYamlInvalid a deriving (Generic)

instance (Show a) => Show (ParseError a) where
  show (ConfettiYamlInvalid a) = "There was an issue parsing your ~/.confetti.yml: " ++ show a
  show (GroupNotFound a) = printf "No match for group %s found in your ~/.confetti.yml: " (show a)
  show ConfettiYamlNotFound = "No confetti spec file found! See https://github.com/aviaviavi/confetti if you need help setting one up"

-- Errors from applying our config spec
newtype ApplyError a = VariantsMissing [a]
instance (Show a) => Show (ApplyError a)  where
  show (VariantsMissing a) = "Couldn't find one or more of your variant files to use: " ++ show a

type ConfigVariant = String
type ConfigTarget = String
data ConfigGroup = ConfigGroup
  { name    :: T.Text
  , targets :: [FilePath]
  } deriving (Show, Generic)

newtype ParsedSpecFile = ParsedSpecFile
  { groups :: [ConfigGroup]
  } deriving (Show, Generic)

instance ToJSON ConfigGroup
instance ToJSON ParsedSpecFile
instance FromJSON ConfigGroup
instance FromJSON ParsedSpecFile

data ConfigSpec = ConfigSpec
  { configGroup   :: ConfigGroup
  , configVariant :: ConfigVariant
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
          (return . Left . ConfettiYamlInvalid . T.pack . prettyPrintParseException)
          (`findGroup` groupName)
          eitherParsed

-- Finds a given group in a parsed spec file. Returns a GroupNotFound if the group
-- is missing
findGroup :: ParsedSpecFile -> T.Text -> IO  (Either (ParseError T.Text) ConfigGroup)
findGroup spec groupName =
  sequence $ maybe (Left $ GroupNotFound groupName) (Right . expandPathsForGroup) $
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

removeIfExists  :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
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
linkTargets :: ConfigVariant -> [ConfigTarget] -> IO ()
linkTargets variant confTargets =
  let variantPaths = constructFullVariantPaths variant confTargets
  in mapM_
    (\pair -> createSymbolicLink (fst pair) (snd pair :: FilePath))
    (zip variantPaths confTargets)

-- For a given variant and list of targets, construct the full paths of all
-- variants
constructFullVariantPaths :: ConfigVariant -> [ConfigTarget] -> [FilePath]
constructFullVariantPaths variant =
  map
    (\t ->
       Data.List.Utils.join "/" (init (split "/" t)) ++
       "/" ++ variant ++ "." ++ last (split "/" t))

-- Run the spec! Every target will be a symlink to the variant
-- file
applySpec :: ConfigSpec -> IO (Maybe (ApplyError T.Text))
applySpec spec = do
  let groupTargets = targets $ configGroup spec
      variantPaths = constructFullVariantPaths (configVariant spec) groupTargets
  mapM_ backUpIfNonSymLink groupTargets
  mapM_ removeIfExists groupTargets
  missingVariants <- filterMissingVariants variantPaths
  if null missingVariants
    then do
      mapM_
        (\p -> printSuccess $ fst p ++ " -> " ++ snd p)
        (zip groupTargets variantPaths)
      linkTargets (configVariant spec) groupTargets
      return Nothing
    else return $ Just (VariantsMissing (map T.pack missingVariants))

-- Expands ~ to $HOME in a path
absolutePath :: FilePath -> IO FilePath
absolutePath path = do
  home <- getHomeDirectory
  return $ replace "~" home path

-- Prints green
printSuccess :: String -> IO ()
printSuccess s = putStrLn $ "\x1b[32m" ++  s

-- Prints red
printFail :: String -> IO ()
printFail s = putStrLn $ "\x1b[31m" ++  s
