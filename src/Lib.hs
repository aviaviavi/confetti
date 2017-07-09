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
import Data.String.Utils
import           GHC.Generics
import           Prelude               hiding (catch)
import           System.Directory
import           System.IO.Error       hiding (catch)
import           System.Posix.Files
import           Text.Printf

import qualified Data.Text             as T

data ParseError a = ConfettiYamlNotFound | GroupNotFound a | ConfettiYamlInvalid a deriving (Generic)

instance (Show a) => Show (ParseError a) where
  show (ConfettiYamlInvalid a) = "There was an issue parsing your ~/.confetti.yml: " ++ show a
  show (GroupNotFound a) = printf "No match for group %s found in your ~/.confetti.yml: " (show a)
  show ConfettiYamlNotFound = "No confetti spec file found! See https://github.com/aviaviavi/confetti if you need help setting one up"

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
  { configGroup :: ConfigGroup
  , variant     :: ConfigVariant
  }

parseGroup:: FilePath -> T.Text -> IO (Either (ParseError T.Text) ConfigGroup)
parseGroup specFile groupName =
  doesFileExist specFile >>= \exists -> helper specFile groupName exists
  where
    helper specFile groupName exists
      | not exists = return $ Left ConfettiYamlNotFound
      | otherwise = do
        eitherParsed <- decodeFileEither specFile
        either
          (return . Left . ConfettiYamlInvalid . T.pack . prettyPrintParseException)
          (\p -> findGroup p groupName)
          eitherParsed

findGroup :: ParsedSpecFile -> T.Text -> IO  (Either (ParseError T.Text) ConfigGroup)
findGroup spec groupName =
  sequence $ maybe (Left $ GroupNotFound groupName) (Right . expandPathsForGroup) $
  find (\g -> name g == groupName) (groups spec)

expandPathsForGroup group =
  mapM absolutePath (targets group) >>= \expanded ->
    return $ group { targets = expanded }

backUpIfNonSymLink :: FilePath -> IO ()
backUpIfNonSymLink file = do
  exists <- doesFileExist file
  isLink <- if exists then pathIsSymbolicLink file else return False
  when (exists && not isLink) $ createBackup file

createBackup :: FilePath -> IO ()
createBackup file =
  let newName =
        (round <$> getPOSIXTime) >>=
        (\t -> return $ file ++ "." ++ show t ++ ".backup")
  in newName >>= \backup -> copyFile file backup

removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

filterMissingVariants :: [ConfigVariant] -> IO [FilePath]
filterMissingVariants paths = filterM (fmap not . doesFileExist) paths

linkTargets :: ConfigVariant -> [ConfigTarget] -> IO ()
linkTargets variant targets =
  let variantPaths = constructFullVariantPaths variant targets
  in mapM_
    (\pair -> createSymbolicLink (fst pair) (snd pair :: FilePath))
    (zip variantPaths targets)


constructFullVariantPaths :: ConfigVariant -> [ConfigTarget] -> [FilePath]
constructFullVariantPaths variant targets =
  map
    (\t ->
       (Data.List.Utils.join "/" $ init (split "/" t)) ++
       "/" ++ variant ++ "." ++ (last $ split "/" t))
    targets

applySpec :: ConfigSpec -> IO (Maybe (ApplyError T.Text))
applySpec spec = do
  let groupTargets = targets $ configGroup spec
      variantPaths = constructFullVariantPaths (variant spec) groupTargets
  mapM_ backUpIfNonSymLink groupTargets
  mapM_ removeIfExists groupTargets
  missingVariants <- filterMissingVariants variantPaths
  if null missingVariants
    then do
      mapM_
        (\p -> printSuccess $ fst p ++ " -> " ++ snd p)
        (zip groupTargets variantPaths)
      linkTargets (variant spec) groupTargets
      return Nothing
    else return $ Just (VariantsMissing (map T.pack missingVariants))

absolutePath :: FilePath -> IO FilePath
absolutePath path = do
  home <- getHomeDirectory
  return $ replace "~" home path

printSuccess s = putStrLn $ "\x1b[32m" ++  s
printFail s = putStrLn $ "\x1b[31m" ++  s
