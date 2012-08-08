-- Copyright 2012 Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- | According to
-- https://github.com/cucumber/gherkin/wiki/BNF/cd25abae017ac768e1d39f80aabf733642515889
-- with the following considerations:
--
--   * There's no need of 'white' after 'tags', 'comment' or 'steps'
--
--   * 'lineToEol' and 'keywordSpace' are not implemented on the original BNF.
--
--   * There are two versions of 'step'.  This module uses the first, which
--     seems to be more complete.
--
--   * 'cell' is defined as @[^\r\n|]+ \'|\'@ instead of @[^\r\n|] \'|\'@
--
--   * 'row' should start with white
module
  Language.Abacate.BNF
  (module Language.Abacate.Types, abacate)
  where

-- base
import Prelude hiding (unlines)
import Control.Applicative hiding (many, (<|>), optional)
import Control.Monad

-- text
import Data.Text hiding (map)

-- parsec
import Text.Parsec
import Text.Parsec.Text

-- abacate
import Language.Abacate.Types

abacate :: GenParser st Abacate
abacate = feature

feature :: GenParser st Feature
feature
  = do
    white
    pBegComment <- comment
    pTags <- tags
    pHeader <- header
    pBackground <- option Nothing $ Just <$> try background
    pFeatureElements <- featureElements
    pEndComment <- comment
    return
      $ Feature
        pBegComment
        pTags
        pHeader
        pBackground
        pFeatureElements
        pEndComment

header :: GenParser st Header
header
  = strip
    <$> pack
    <$> manyTill anyChar
      (lookAhead
         $ choice
         $ map try [void scenarioOutline, void scenario, void background])

featureElements :: GenParser st FeatureElements
featureElements
  = many $ (FES <$> try scenario) <|> (FESO <$> try scenarioOutline)

scenario :: GenParser st Scenario
scenario
  = do
    pComment <- comment
    pTags <- tags
    scenarioKeyword
    (name, pSteps) <- scenarioCommon
    return $ Scenario pTags $ BasicScenario pComment name pSteps

scenarioOutline :: GenParser st ScenarioOutline
scenarioOutline
  = do
    pComment <- comment
    pTags <- tags
    scenarioOutlineKeyword
    (name, pSteps) <- scenarioCommon
    pExamplesSection <- examplesSection
    white
    return
      $ ScenarioOutline pExamplesSection
      $ Scenario pTags
      $ BasicScenario pComment name pSteps

background :: GenParser st Background
background
  = do
    pComment <- comment
    backgroundKeyword
    (name, pSteps) <- scenarioCommon
    return $ BasicScenario pComment name pSteps

tags :: GenParser st Tags
tags
  = white
    >> many
      (do
        pTag <- tag
        white1
        return pTag)

tag :: GenParser st Tag
tag = char '@' >> pack <$> many1 (noneOf "@\r\n\t ")

comment :: GenParser st Comment
comment
  = unlines
    <$> many
      (do
        pComment <- commentLine
        white
        return pComment)

commentLine :: GenParser st Comment
commentLine = gSpaces >> char '#' >> lineToEol

steps :: GenParser st Steps
steps = many $ try step

step :: GenParser st Step
step
 = do
   pComment <- comment
   gSpaces
   keyword <- stepKeyword
   keywordSpace
   body <- lineToEol
   void (many1 eol) <|> eof
   pMultilineArg <- option Nothing $ Just <$> try multilineArg
   white
   return $ Step pComment keyword body pMultilineArg

examplesSection :: GenParser st ExamplesSection
examplesSection = many $ try examples

examples :: GenParser st Examples
examples
  = do
    pComment <- comment
    gSpaces
    examplesKeyword
    gSpaces
    name <- linesToKeyword
    eol
    pTable <- table
    white
    return $ Examples pComment name pTable

multilineArg :: GenParser st MultilineArg
multilineArg = MAT <$> try table <|> MAPS <$> try pyString

pyString :: GenParser st PyString
pyString
  = openPyString >> strip <$> pack <$> manyTill anyChar (try closePyString)

openPyString :: GenParser st ()
openPyString = gSpaces >> string "\"\"\"" >> gSpaces >> eol

closePyString :: GenParser st ()
closePyString = eol >> gSpaces >> string "\"\"\"" >> white

cell :: GenParser st Cell
cell
  = do
    pCell <- many1 (noneOf "\r\n|")
    void (char '|')
    return $ strip $ pack pCell

row :: GenParser st Row
row
  = do
    white
    void $ char '|'
    pRow <- many1 $ try cell
    eol
    return pRow

table :: GenParser st Table
table = many1 $ try row

stepKeyword :: GenParser st StepKeyword
stepKeyword
  = read
    <$> choice
      (map (try . string . show) ([minBound .. maxBound] :: [StepKeyword]))

examplesKeyword :: GenParser st ()
examplesKeyword = void $ string "Examples:"

scenarioOutlineKeyword :: GenParser st ()
scenarioOutlineKeyword = void $ string "Scenario Outline:"

scenarioKeyword :: GenParser st ()
scenarioKeyword = void $ string "Scenario:"

backgroundKeyword :: GenParser st ()
backgroundKeyword = void $ string "Background:"

linesToKeyword :: GenParser st Text
linesToKeyword
  = strip
    <$> pack
    <$> manyTill
      anyChar
      (lookAhead $ try $ eol >> gSpaces >> reservedWordsAndSymbols)

reservedWordsAndSymbols :: GenParser st ()
reservedWordsAndSymbols
  = choice
    $ map
      try
      [stepKeyword >> keywordSpace,
        scenarioKeyword,
        scenarioOutlineKeyword,
        void table,
        void tag,
        void commentLine]

gSpace :: GenParser st ()
gSpace = void $ oneOf " \t"

eol :: GenParser st ()
eol = optional (char '\r') >> void (char '\n')

white :: GenParser st ()
white = skipMany oneWhite

-- Should be in BNF

lineToEol :: GenParser st Text
lineToEol = pack <$> many (noneOf "\r\n")

keywordSpace :: GenParser st ()
keywordSpace = void $ char ' '

-- Not defined in BNF

oneWhite :: GenParser st ()
oneWhite = gSpace <|> eol

white1 :: GenParser st ()
white1 = skipMany1 oneWhite

gSpaces :: GenParser st ()
gSpaces = skipMany gSpace

scenarioCommon :: GenParser st (Text, Steps)
scenarioCommon
  = do
    gSpaces
    name <- linesToKeyword
    white
    pSteps <- steps
    return (name, pSteps)
