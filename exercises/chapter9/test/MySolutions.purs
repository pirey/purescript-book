module Test.MySolutions where

import Prelude

import Affjax.Node as AN
import Affjax.ResponseFormat as ResponseFormat
import Control.Parallel (parOneOf, parTraverse)
import Data.Array (concat, (:))
import Data.Either (Either(..), hush)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), length, split)
import Data.Traversable (traverse)
import Effect.Aff (Aff, Error, Milliseconds(..), attempt, delay)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)
import Node.Path as Path

concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles in1 in2 out = do
  content1 <- readTextFile UTF8 in1
  content2 <- readTextFile UTF8 in2
  writeTextFile UTF8 out (content1 <> content2)
  pure unit

concatenateMany :: Array FilePath -> FilePath -> Aff Unit
concatenateMany arr out = do
  arrContents <- traverse (readTextFile UTF8) arr
  writeTextFile UTF8 out $ fold arrContents

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters file =
  attempt do
    contents <- readTextFile UTF8 file
    pure $ length contents

getUrl :: String -> Aff String
getUrl url = do
  result <- AN.get ResponseFormat.string url
  pure case result of
    Left err -> "GET /api response failed to decode: " <> AN.printError err
    Right response -> response.body

writeGet :: String -> FilePath -> Aff Unit
writeGet url out = do
  body <- getUrl url
  writeTextFile UTF8 out body
  pure unit

concatenateManyParallel :: Array FilePath -> FilePath -> Aff Unit
concatenateManyParallel arr out = do
  arrContents <- parTraverse (readTextFile UTF8) arr
  writeTextFile UTF8 out $ fold arrContents

getWithTimeout :: Number -> String -> Aff (Maybe String)
getWithTimeout ms url =
  parOneOf
    [ AN.get ResponseFormat.string url <#> hush <#> map _.body
    , delay (Milliseconds ms) $> Nothing
    ]

recurseFiles :: FilePath -> Aff (Array FilePath)
recurseFiles file = do
  contents <- readTextFile UTF8 file
  case contents of
    "" -> pure [ file ]
    c -> do
      let
        dir = Path.dirname file

        files = split (Pattern "\n") contents

        filesFromRoot = map (\f -> Path.concat [ dir, f ]) files
      arrarr <- parTraverse recurseFiles filesFromRoot
      pure $ file : concat arrarr
