module Test.MySolutions where

import Prelude

import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.State (State, execState, modify_)
import Control.Monad.Writer (Writer, tell)
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence, traverse_)

testParens :: String -> Boolean
testParens str =
  let
    openTally :: Char -> Int -> Int
    -- Open parens only considered if not already in deficit.
    -- No recovery from too-many closed parens.
    openTally '(' tally | tally >= 0 = tally + 1
    openTally ')' tally = tally - 1
    -- Non-parens has no effect
    openTally _ tally = tally

    sumParens :: Array Char -> State Int Unit
    sumParens = traverse_ \c -> modify_ $ openTally c

    finalTally :: Int
    finalTally = execState (sumParens $ toCharArray str) 0
  in
    finalTally == 0

--

type Level = Int
type Doc = Reader Level String

line :: String -> Doc
line str = do
  level <- ask
  pure $ (power "  " level) <> str

indent :: Doc -> Doc
indent = local $ (+) 1

cat :: Array Doc -> Doc
cat = sequence >=> joinWith "\n" >>> pure

render :: Doc -> String
render doc = runReader doc 0

--

sumArrayWriter :: Array Int -> Writer (Additive Int) Unit
sumArrayWriter = traverse_ \n -> do
  tell $ Additive n
  pure unit

