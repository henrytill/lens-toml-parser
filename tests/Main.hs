{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Main (main) where

import           Control.Monad          (unless)
import qualified Data.Map.Lazy          as Map
import qualified Data.Text              as T
import           Data.Text.IO           (readFile)
import           Lens.Family2
import           Lens.Family2.Stock     (at, just_)
import           Lens.Family2.Unchecked (adapter)
import           Prelude                hiding (readFile)
import           System.Exit            (exitFailure)
import           Test.Dwergaz
import qualified TOML
import           TOML.Lens


allEqual :: Eq a => [a] -> Bool
allEqual (x:xs) = all (== x) xs
allEqual []     = error "allEqual: empty list"

iso :: (a -> b) -> (b' -> a') -> Lens a a' b b'
iso ctor dtor = under (adapter ctor dtor)

alist
  :: (Ord k1, Ord k2, Functor f)
  => LensLike f [(k1, v1)] [(k2, v2)] (Map.Map k1 v1) (Map.Map k2 v2)
alist = iso Map.fromList Map.toList

mapAt
  :: Applicative f
  => T.Text
  -> (Map.Map T.Text TOML.Value -> f (Map.Map T.Text TOML.Value))
  -> Map.Map T.Text TOML.Value
  -> f (Map.Map T.Text TOML.Value)
mapAt k = at k . just_ . _Table . alist

listAt
  :: Applicative f
  => T.Text
  -> ([TOML.Value] -> f [TOML.Value])
  -> Map.Map T.Text TOML.Value
  -> f (Map.Map T.Text TOML.Value)
listAt k = at k . just_ . _List

testTableKey :: [(T.Text, TOML.Value)] -> Test
testTableKey kv
  = Expect "'key' from 'table' == Just \"value\""
           (==) expected actual
  where
    expected = Just "value"
    actual   = kv ^? alist . mapAt "table" . at "key" . just_ . _String

testTableZoo :: [(T.Text, TOML.Value)] -> Test
testTableZoo kv
  = Expect "'zoo' from 'table' == Nothing"
           (==) expected actual
  where
    expected = Nothing
    actual   = kv ^? alist . mapAt "table" . at "zoo" . just_ . _String

testTableSubtableKey :: [(T.Text, TOML.Value)] -> Test
testTableSubtableKey kv
  = Expect "'key' from 'subtable' from 'table' == Just \"another value\""
           (==) expected actual
  where
    expected = Just "another value"
    actual   = kv ^? alist . mapAt "table" . mapAt "subtable" . at "key" . just_ . _String

testTableInlineNameFirst :: [(T.Text, TOML.Value)] -> Test
testTableInlineNameFirst kv
  = Expect "'first' from 'name' from 'inline' from 'table' == \"Tom\""
           (==) expected actual
  where
    expected = Just "Tom"
    actual   = kv ^? alist . mapAt "table" . mapAt "inline" . mapAt "name" . at "first" . just_ . _String

testTableInlinePointY :: [(T.Text, TOML.Value)] -> Test
testTableInlinePointY kv
  = Expect "'y' from 'point' from 'inline' from 'table' == Just 2"
           (==) expected actual
  where
    expected = Just 2
    actual   = kv ^? alist . mapAt "table" . mapAt "inline" . mapAt "point" . at "y" . just_ . _Integer

testStringBasicBasic :: [(T.Text, TOML.Value)] -> Test
testStringBasicBasic kv
  = Expect "'basic' from 'basic' from 'string' == <some escaped nonsense>"
           (==) expected actual
  where
    expected = Just "I'm a string. \"You can quote me\". Name\tJos\233\nLocation\tSF."
    actual   = kv ^? alist . mapAt "string" . mapAt "basic" . at "basic" . just_ . _String

testStringMultiline :: [(T.Text, TOML.Value)] -> Test
testStringMultiline kv
  = Predicate "'key1', 'key2', and 'key3' from 'multiline' from 'string' are all the same"
              allEqual
              [actual1, actual2, actual3]
  where
    actual1 = kv ^? alist . mapAt "string" . mapAt "multiline" . at "key1" . just_ . _String
    actual2 = kv ^? alist . mapAt "string" . mapAt "multiline" . at "key2" . just_ . _String
    actual3 = kv ^? alist . mapAt "string" . mapAt "multiline" . at "key3" . just_ . _String

testStringMultilineContinued :: [(T.Text, TOML.Value)] -> Test
testStringMultilineContinued kv
  = Predicate "'key1', 'key2', and 'key3' from 'continued' from 'multiline' from 'string' are all the same"
              allEqual
              [actual1, actual2, actual3]
  where
    actual1 = kv ^? alist . mapAt "string" . mapAt "multiline" . mapAt "continued" . at "key1" . just_ . _String
    actual2 = kv ^? alist . mapAt "string" . mapAt "multiline" . mapAt "continued" . at "key2" . just_ . _String
    actual3 = kv ^? alist . mapAt "string" . mapAt "multiline" . mapAt "continued" . at "key3" . just_ . _String

testArrayKey1 :: [(T.Text, TOML.Value)] -> Test
testArrayKey1 kv
  = Expect "'key1' from 'array' == [1, 2, 3]"
           (==) expected actual
  where
    expected = [1, 2, 3]
    actual   = kv ^.. alist . mapAt "array" . listAt "key1" . traverse . _Integer

runTests :: [(T.Text, TOML.Value)] -> [Result]
runTests kv = runTest . ($ kv) <$> tests
  where
    tests = [ testTableKey
            , testTableZoo
            , testTableSubtableKey
            , testTableInlineNameFirst
            , testTableInlinePointY
            , testStringBasicBasic
            , testStringMultiline
            , testStringMultilineContinued
            , testArrayKey1
            ]

readTOMLFile :: String -> IO [(T.Text, TOML.Value)]
readTOMLFile file = readFile file >>= parse >>= handleError
  where
    parse       = pure . TOML.parseTOML
    handleError = either (error . show) pure

main :: IO ()
main =  do
  ex <- readTOMLFile "./example/example-v0.4.0.toml"
  let rs = runTests ex
  mapM_ print rs
  unless (all isPassed rs) exitFailure
