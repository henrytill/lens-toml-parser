{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad (unless)
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import           Data.Text.IO (readFile)
import           Lens.Simple
import           Prelude hiding (readFile)
import           System.Exit (exitFailure)
import           Test.Dwergaz
import qualified TOML
import           TOML.Lens


alist :: Ord k => [(k, v)] -> Map.Map k v
alist = Map.fromList

tableAt :: T.Text -> Map.Map T.Text TOML.Value -> Map.Map T.Text TOML.Value
tableAt k = views (at k . _Just . _Table) alist

test1 :: [(T.Text, TOML.Value)] -> Test
test1 kv = Expect "get key1" (==) expected actual
  where
    expected = [1, 2, 3]
    actual   = toListOf (at "key1" . _Just . _List . traverse . _Integer) (tableAt "array" (alist kv))

runTests :: [(T.Text, TOML.Value)] -> IO [Result]
runTests kv = pure (runTest <$> [test1 kv])

readTOMLFile :: String -> IO [(T.Text, TOML.Value)]
readTOMLFile file = readExFile >>= parse >>= handleError
  where
    readExFile  = readFile file
    parse       = pure . TOML.parseTOML
    handleError = either (error . show) pure

main :: IO ()
main =  do
  ex <- readTOMLFile "./example/example-v0.4.0.toml"
  rs <- runTests ex
  _  <- mapM_ print rs
  unless (all isPassed rs) exitFailure
