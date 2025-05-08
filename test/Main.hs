{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Lens.Family2
import Lens.Family2.Stock (at, just_, _2)
import Lens.Family2.Unchecked (adapter)
import System.Exit (exitFailure)
import Test.Dwergaz
import Toml (Table, Table' (..), Value, Value')
import qualified Toml
import Toml.Lens

allEqual :: (Eq a) => [a] -> Bool
allEqual (x : xs) = all (== x) xs
allEqual [] = error "allEqual: empty list"

table :: Adapter' (Table' a) (Map Text (a, Value' a))
table = adapter unTable MkTable
  where
    unTable (MkTable t) = t

valueAt ::
  (Applicative f) =>
  Text ->
  (Value -> f Value) ->
  Table ->
  f Table
valueAt k = under table . at k . just_ . _2

mapAt ::
  (Applicative f) =>
  Text ->
  (Table -> f Table) ->
  Table ->
  f Table
mapAt k = valueAt k . _Table

testTableKey :: Table -> Test
testTableKey tbl =
  assertEqual
    "'key' from 'table' == Just \"value\""
    expected
    actual
  where
    expected = Just "value"
    actual = tbl ^? mapAt "table" . valueAt "key" . _Text

testTableZoo :: Table -> Test
testTableZoo tbl =
  assertEqual
    "'zoo' from 'table' == Nothing"
    expected
    actual
  where
    expected = Nothing
    actual = tbl ^? mapAt "table" . valueAt "zoo" . _Text

testTableSubtableKey :: Table -> Test
testTableSubtableKey tbl =
  assertEqual
    "'key' from 'subtable' from 'table' == Just \"another value\""
    expected
    actual
  where
    expected = Just "another value"
    actual = tbl ^? mapAt "table" . mapAt "subtable" . valueAt "key" . _Text

testTableInlineNameFirst :: Table -> Test
testTableInlineNameFirst tbl =
  assertEqual
    "'first' from 'name' from 'inline' from 'table' == \"Tom\""
    expected
    actual
  where
    expected = Just "Tom"
    actual = tbl ^? mapAt "table" . mapAt "inline" . mapAt "name" . valueAt "first" . _Text

testTableInlinePointY :: Table -> Test
testTableInlinePointY tbl =
  assertEqual
    "'y' from 'point' from 'inline' from 'table' == Just 2"
    expected
    actual
  where
    expected = Just 2
    actual = tbl ^? mapAt "table" . mapAt "inline" . mapAt "point" . valueAt "y" . _Integer

testStringBasicBasic :: Table -> Test
testStringBasicBasic tbl =
  assertEqual
    "'basic' from 'basic' from 'string' == <some escaped nonsense>"
    expected
    actual
  where
    expected = Just "I'm a string. \"You can quote me\". Name\tJos\233\nLocation\tSF."
    actual = tbl ^? mapAt "string" . mapAt "basic" . valueAt "basic" . _Text

testStringMultiline :: Table -> Test
testStringMultiline tbl =
  assertBool
    "'key1', 'key2', and 'key3' from 'multiline' from 'string' are all the same"
    (allEqual [actual1, actual2, actual3])
  where
    actual1 = tbl ^? mapAt "string" . mapAt "multiline" . valueAt "key1" . _Text
    actual2 = tbl ^? mapAt "string" . mapAt "multiline" . valueAt "key2" . _Text
    actual3 = tbl ^? mapAt "string" . mapAt "multiline" . valueAt "key3" . _Text

testStringMultilineContinued :: Table -> Test
testStringMultilineContinued tbl =
  assertBool
    "'key1', 'key2', and 'key3' from 'continued' from 'multiline' from 'string' are all the same"
    (allEqual [actual1, actual2, actual3])
  where
    actual1 = tbl ^? mapAt "string" . mapAt "multiline" . mapAt "continued" . valueAt "key1" . _Text
    actual2 = tbl ^? mapAt "string" . mapAt "multiline" . mapAt "continued" . valueAt "key2" . _Text
    actual3 = tbl ^? mapAt "string" . mapAt "multiline" . mapAt "continued" . valueAt "key3" . _Text

testArrayKey1 :: Table -> Test
testArrayKey1 tbl =
  assertEqual
    "'key1' from 'array' == [1, 2, 3]"
    expected
    actual
  where
    expected = [1, 2, 3]
    actual = tbl ^.. mapAt "array" . valueAt "key1" . _List . traverse . _Integer

tests :: [Table -> Test]
tests =
  [ testTableKey,
    testTableZoo,
    testTableSubtableKey,
    testTableInlineNameFirst,
    testTableInlinePointY,
    testStringBasicBasic,
    testStringMultiline,
    testStringMultilineContinued,
    testArrayKey1
  ]

step :: Result -> (ShowS, Bool) -> (ShowS, Bool)
step result (f, allPassed) =
  ( showString (resultToString result) . showChar '\n' . f,
    resultIsPassed result && allPassed
  )

runTests :: Table -> (String, Bool)
runTests tbl = (buildString mempty, passed)
  where
    (buildString, passed) = foldr (step . runTest . ($ tbl)) (mempty, True) tests

readTomlFile :: String -> IO Table
readTomlFile file = TIO.readFile file >>= parse >>= handleError
  where
    parse = pure . Toml.parse
    handleError = either (error . show) (pure . Toml.forgetTableAnns)

main :: IO ()
main = do
  ex <- readTomlFile "./example/example-v0.4.0.toml"
  let (output, passed) = runTests ex
  putStr output
  unless passed exitFailure
