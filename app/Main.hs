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
  { groupName   :: String
  , variantName :: String
  } deriving (Show)

-- Parse our command line arguments for a confetti command,
-- or display some information and exit
parseMainArgs :: [String] -> IO MainArgs
parseMainArgs [g, v] = return (MainArgs g v)
parseMainArgs a =
  let usage = "Usage: `confetti $group_name $variant_name`"
      showHelp = (putStrLn usage >> exitSuccess)
      confettiVersion = putStrLn ("confetti " ++ showVersion version) >> exitSuccess
  in case a of
       ["-h"]        -> showHelp
       ["--help"]    -> showHelp
       ["-v"]        -> confettiVersion
       ["--version"] -> confettiVersion
       _             -> printFail usage >> exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  parsed <- parseMainArgs args
  printf "Setting %s to %s\n" (groupName parsed) (variantName parsed)
  specPath <- absolutePath "~/.confetti.yml"
  group <- parseGroup specPath (T.pack $ groupName parsed)
  either (printFail . show) (runSpec (variantName parsed)) group

-- Run our configuration spec. Prints whatever error message we get back,
-- or a success message
runSpec :: ConfigVariant -> ConfigGroup -> IO ()
runSpec variant group =
  applySpec (ConfigSpec group variant) >>= \maybeError ->
    maybe (printSuccess "Success (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧") (printFail . show) maybeError
