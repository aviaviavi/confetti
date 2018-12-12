{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

import           Confetti

import           Data.Maybe
import           Data.Version                    (showVersion)
import           Paths_confetti                  (version)
import           System.Console.CmdArgs
import           System.Console.CmdArgs.Implicit
import           System.Environment
import           System.Exit
import           Text.Printf

import qualified Data.Text                       as T

-- Structure for our command line arguments
data MainArgs = MainArgs
  { group_name     :: String
  , variant_prefix :: ConfigVariantPrefix
  , force          :: Bool
  } deriving (Show, Data, Typeable, Eq)

argParser :: MainArgs
argParser =
  MainArgs
  { group_name = def &= argPos 0 &= typ "GROUP_NAME"
  , variant_prefix = def &= argPos 1 &= typ "VARIANT_PREFIX" &= opt (Nothing :: Maybe T.Text)
  , force = def &= help "If the target already exists as a regular file, back it up and then create a link"
  } &=
  summary ("confetti " ++ showVersion version) &=
  program "confetti" &=
  verbosity &=
  help "Easily swap groups of config files"

confettiArgs :: IO MainArgs
confettiArgs = cmdArgs argParser

main :: IO ()
main = do
  parsed <- confettiArgs
  printf
    "Setting %s to %s\n"
    (group_name parsed)
    (showPrefix $ variant_prefix parsed)
  specPath <- absolutePath "~/.confetti.yml"
  group <- parseGroup specPath (T.pack $ group_name parsed)
  let validatedGroup = group >>= validateSpec
  either (printFail . show) (runSpec (variant_prefix parsed) (force parsed)) validatedGroup

-- Run our configuration spec. Prints whatever error message we get back,
-- or a success message
runSpec :: ConfigVariantPrefix -> Bool -> ConfigGroup -> IO ()
runSpec prefix shouldForce group =
  applySpec (ConfigSpec group prefix shouldForce) >>= \maybeError ->
    maybe (printSuccess "Success (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧") (printFail . show) maybeError
