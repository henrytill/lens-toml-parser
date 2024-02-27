{-# Language OverloadedStrings #-}
module Main (main) where

import Control.Monad (unless)
import qualified Data.Text.IO as Text
import Lens.Family2
import Lens.Family2.Stock (at, just_, _2)
import System.Exit (exitFailure)
import Test.Dwergaz
import Toml (Table, Value)
import qualified Toml
import Toml.Lens
import Data.Text (Text)

allEqual :: (Eq a) => [a] -> Bool
allEqual (x : xs) = all (== x) xs
allEqual [] = error "allEqual: empty list"

mapAt ::
  (Applicative f) =>
  Text ->
  (Table -> f Table) ->
  Table ->
  f Table
mapAt k = valueAt k . _Table

arrayAt ::
  (Applicative f) =>
  Text ->
  ([Value] -> f [Value]) ->
  Table ->
  f Table
arrayAt k = valueAt k . _List

valueAt ::
  (Applicative f) =>
  Text ->
  (Value -> f Value) ->
  Table ->
  f Table
valueAt k f (Toml.MkTable t) = Toml.MkTable <$> (at k . just_ . _2) f t

testTableKey :: Table -> Test
testTableKey kv =
  Expect
    "'key' from 'table' == Just \"value\""
    (==)
    expected
    actual
  where
    expected = Just "value"
    actual = kv ^? mapAt "table" . valueAt "key" . _Text

testTableZoo :: Table -> Test
testTableZoo kv =
  Expect
    "'zoo' from 'table' == Nothing"
    (==)
    expected
    actual
  where
    expected = Nothing
    actual = kv ^? mapAt "table" . valueAt "zoo" . _Text

testTableSubtableKey :: Table -> Test
testTableSubtableKey kv =
  Expect
    "'key' from 'subtable' from 'table' == Just \"another value\""
    (==)
    expected
    actual
  where
    expected = Just "another value"
    actual = kv ^? mapAt "table" . mapAt "subtable" . valueAt "key" . _Text

testTableInlineNameFirst :: Table -> Test
testTableInlineNameFirst kv =
  Expect
    "'first' from 'name' from 'inline' from 'table' == \"Tom\""
    (==)
    expected
    actual
  where
    expected = Just "Tom"
    actual = kv ^? mapAt "table" . mapAt "inline" . mapAt "name" . valueAt "first" . _Text

testTableInlinePointY :: Table -> Test
testTableInlinePointY kv =
  Expect
    "'y' from 'point' from 'inline' from 'table' == Just 2"
    (==)
    expected
    actual
  where
    expected = Just 2
    actual = kv ^? mapAt "table" . mapAt "inline" . mapAt "point" . valueAt "y" . _Integer

testStringBasicBasic :: Table -> Test
testStringBasicBasic kv =
  Expect
    "'basic' from 'basic' from 'string' == <some escaped nonsense>"
    (==)
    expected
    actual
  where
    expected = Just "I'm a string. \"You can quote me\". Name\tJos\233\nLocation\tSF."
    actual = kv ^? mapAt "string" . mapAt "basic" . valueAt "basic" . _Text

testStringMultiline :: Table -> Test
testStringMultiline kv =
  Predicate
    "'key1', 'key2', and 'key3' from 'multiline' from 'string' are all the same"
    allEqual
    [actual1, actual2, actual3]
  where
    actual1 = kv ^? mapAt "string" . mapAt "multiline" . valueAt "key1" . _Text
    actual2 = kv ^? mapAt "string" . mapAt "multiline" . valueAt "key2" . _Text
    actual3 = kv ^? mapAt "string" . mapAt "multiline" . valueAt "key3" . _Text

testStringMultilineContinued :: Table -> Test
testStringMultilineContinued kv =
  Predicate
    "'key1', 'key2', and 'key3' from 'continued' from 'multiline' from 'string' are all the same"
    allEqual
    [actual1, actual2, actual3]
  where
    actual1 = kv ^? mapAt "string" . mapAt "multiline" . mapAt "continued" . valueAt "key1" . _Text
    actual2 = kv ^? mapAt "string" . mapAt "multiline" . mapAt "continued" . valueAt "key2" . _Text
    actual3 = kv ^? mapAt "string" . mapAt "multiline" . mapAt "continued" . valueAt "key3" . _Text

testArrayKey1 :: Table -> Test
testArrayKey1 kv =
  Expect
    "'key1' from 'array' == [1, 2, 3]"
    (==)
    expected
    actual
  where
    expected = [1, 2, 3]
    actual = kv ^.. mapAt "array" . arrayAt "key1" . traverse . _Integer

runTests :: Table -> [Result]
runTests kv = runTest . ($ kv) <$> tests
  where
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

readTomlFile :: String -> IO Table
readTomlFile file = Text.readFile file >>= parse >>= handleError
  where
    parse = pure . Toml.parse
    handleError = either (error . show) (pure . Toml.forgetTableAnns)

main :: IO ()
main = do
  ex <- readTomlFile "./example/example-v0.4.0.toml"
  let rs = runTests ex
  mapM_ print rs
  unless (all isPassed rs) exitFailure
