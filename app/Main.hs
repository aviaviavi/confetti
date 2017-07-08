{-# LANGUAGE OverloadedStrings   #-}

import           Lib
import           System.Environment
import           System.Exit
import           Text.Printf
import qualified Data.Text as T

data MainArgs = MainArgs
  { groupName   :: String
  , variantName :: String
  } deriving (Show)

parseMainArgs :: [String] -> IO MainArgs
parseMainArgs [g, v] = return (MainArgs g v)
parseMainArgs _ =
  putStrLn "Usage: confetti group_name variant_name" >> exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  parsed <- parseMainArgs args
  printf "Setting %s to %s\n" (groupName parsed) (variantName parsed)
  group <- parseGroup "/Users/avipress/side/confetti/.confetti.yml" (T.pack $ groupName parsed)
  either (putStrLn . (++ " left") . show) (runSpec (variantName parsed)) group
  putStrLn "done"

runSpec variant group = do
  putStrLn .  (++ " right") . show $ group
  applySpec $ ConfigSpec group variant
  putStr "applied"

