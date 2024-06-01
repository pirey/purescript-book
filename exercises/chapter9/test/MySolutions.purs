module Test.MySolutions where

import Prelude

import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)

concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles in1 in2 out = do
  content1 <- readTextFile UTF8 in1
  content2 <- readTextFile UTF8 in2
  writeTextFile UTF8 out (content1 <> content2)
  pure unit
