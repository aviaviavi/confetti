{-# LANGUAGE OverloadedStrings #-}

import           Lib
import           System.Environment
import           System.Exit
import           Text.Printf

import qualified Data.Text          as T

data MainArgs = MainArgs
  { groupName   :: String
  , variantName :: String
  } deriving (Show)

parseMainArgs :: [String] -> IO MainArgs
parseMainArgs [g, v] = return (MainArgs g v)
parseMainArgs a = let usage = "Usage: `confetti $group_name $variant_name`" in
  case a of
    ["-h"] -> putStrLn usage >> exitSuccess
    _    -> printFail usage >> exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  parsed <- parseMainArgs args
  printf "Setting %s to %s\n" (groupName parsed) (variantName parsed)
  group <-
    parseGroup
      "/Users/avipress/side/confetti/.confetti.yml"
      (T.pack $ groupName parsed)
  either (printFail . show) (runSpec (variantName parsed)) group

runSpec variant group =
  applySpec (ConfigSpec group variant) >>= \maybeError ->
    maybe (printSuccess "Success (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧") (printFail . show) maybeError
