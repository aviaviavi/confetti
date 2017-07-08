{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import           Data.Either
import           Data.List
import           Data.List.Utils
import           Data.Yaml
import           GHC.Generics
import           System.Directory
import  System.Posix.Files

import qualified Data.Text        as T

data ParseError a = ConfettiYamlNotFound | GroupNotFound a | ConfettiYamlInvalid a deriving (Show, Generic)
data ApplyError a = VariantsMissing [a] deriving (Show) -- TODO

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
parseGroup specFile groupName = doesFileExist specFile >>= \exists ->
  helper specFile groupName exists where
    helper specFile groupName exists
      | not exists = return $ Left ConfettiYamlNotFound
      | otherwise = do
          eitherParsed <- decodeFileEither specFile
          return $ either (Left . ConfettiYamlInvalid . T.pack . prettyPrintParseException) (\p -> findGroup p groupName) eitherParsed

findGroup :: ParsedSpecFile -> T.Text -> Either (ParseError T.Text) ConfigGroup
findGroup spec groupName =
  maybe
    (Left $ GroupNotFound groupName)
    Right
    $ find (\g -> name g == groupName) (groups spec)

backUpIfNonSymLink :: FilePath -> IO ()
backUpIfNonSymLink file = do
  exists <- doesFileExist file
  isLink <- pathIsSymbolicLink  file
  if exists && not isLink then
    createBackup file
  else
    return ()

createBackup :: FilePath -> IO ()
createBackup file = error "not implemented"

deleteTarget :: FilePath -> IO ()
deleteTarget file = doesFileExist file >>= \exists ->
  if exists then
    removeFile file
  else
    return ()

allVariantsExists :: ConfigVariant -> [ConfigTarget] -> IO Bool
allVariantsExists variant targets =
  let variantPaths = constructFullVariantPaths variant targets in do
    putStrLn $ "variants len: " ++ (show $ length variantPaths)
    mapM_ putStrLn variantPaths
    mapM doesFileExist variantPaths >>= \s ->
      return $ all (== True) s

linkTargets :: ConfigVariant -> [ConfigTarget] -> IO ()
linkTargets variant targets =
  let variantPaths = constructFullVariantPaths variant targets in do
    mapM_ (\pair -> createSymbolicLink (fst pair) (snd pair :: FilePath)) (zip variantPaths targets)


constructFullVariantPaths :: ConfigVariant -> [ConfigTarget] -> [FilePath]
constructFullVariantPaths variant targets =
  map (\t -> (join "/" $ init (split "/" t)) ++ "/" ++ variant ++ "." ++ (last $ split "/" t)) targets

applySpec :: ConfigSpec -> IO (Maybe (ApplyError T.Text))
applySpec spec = do
  let groupTargets = targets $ configGroup spec
  return $ map backUpIfNonSymLink groupTargets
  return $ map deleteTarget groupTargets
  variantsExist <- allVariantsExists (variant spec) groupTargets
  if variantsExist then do
    putStrLn "all variants found, applying..."
    linkTargets (variant spec) groupTargets
    return Nothing
  else do
    putStrLn "missing variants"
    return $ Just (VariantsMissing ["files"])
