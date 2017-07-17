{-# LANGUAGE OverloadedStrings #-}

import           Data.Version       (showVersion)
import           Lib
import           Paths_confetti     (version)
import           System.Environment
import           System.Exit
import           Text.Printf

import qualified Data.Text          as T

-- Structure for our command line arguments
data MainArgs = MainArgs
  { groupName     :: String
  , variantPrefix :: ConfigVariantPrefix
  } deriving (Show)

-- Parse our command line arguments for a confetti command,
-- or display some information and exit
parseMainArgs :: [String] -> IO MainArgs
parseMainArgs [g, v] = return (MainArgs g $ Just v)
parseMainArgs a =
  let usage =
        "Usage: `confetti [required group_name] [optional variant_prefix]`"
      showHelp = (putStrLn usage >> exitSuccess)
      confettiVersion =
        putStrLn ("confetti " ++ showVersion version) >> exitSuccess
  in case a of
       ["-h"]        -> showHelp
       ["--help"]    -> showHelp
       ["-v"]        -> confettiVersion
       ["--version"] -> confettiVersion
       [g]           -> return (MainArgs g Nothing)
       _             -> printFail usage >> exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  parsed <- parseMainArgs args
  printf
    "Setting %s to %s\n"
    (groupName parsed)
    (showPrefix $ variantPrefix parsed)
  specPath <- absolutePath "~/.confetti.yml"
  group <- parseGroup specPath (T.pack $ groupName parsed)
  let validatedGroup = group >>= validateSpec
  either (printFail . show) (runSpec (variantPrefix parsed)) validatedGroup

-- Run our configuration spec. Prints whatever error message we get back,
-- or a success message
runSpec :: ConfigVariantPrefix -> ConfigGroup -> IO ()
runSpec prefix group =
  applySpec (ConfigSpec group prefix) >>= \maybeError ->
    maybe (printSuccess "Success (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧") (printFail . show) maybeError
