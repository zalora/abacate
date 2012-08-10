-- |
-- Module      :  Language.Abacate.BNF
-- Copyright   :  (c) Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com> 2012
-- License     :  Apache 2.0 (see the file LICENSE)
--
-- Maintainer  :  Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
-- Stability   :  unstable
-- Portability :  portable
module Language.Abacate.BNF (abacate) where

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

abacate :: Parser Abacate
abacate = feature

feature :: Parser Feature
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

header :: Parser Header
header
  = strip
    <$> pack
    <$> manyTill anyChar
      (lookAhead
         $ choice
         $ map try [void scenarioOutline, void scenario, void background])

featureElements :: Parser FeatureElements
featureElements
  = many $ (FES <$> try scenario) <|> (FESO <$> try scenarioOutline)

scenario :: Parser Scenario
scenario
  = do
    pComment <- comment
    pTags <- tags
    scenarioKeyword
    (name, pSteps) <- scenarioCommon
    return $ Scenario pTags $ BasicScenario pComment name pSteps

scenarioOutline :: Parser ScenarioOutline
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

background :: Parser Background
background
  = do
    pComment <- comment
    backgroundKeyword
    (name, pSteps) <- scenarioCommon
    return $ BasicScenario pComment name pSteps

tags :: Parser Tags
tags
  = white
    >> many
      (do
        pTag <- tag
        white1
        return pTag)

tag :: Parser Tag
tag = char '@' >> pack <$> many1 (noneOf "@\r\n\t ")

comment :: Parser Comment
comment
  = unlines
    <$> many
      (do
        pComment <- commentLine
        white
        return pComment)

commentLine :: Parser Comment
commentLine = gSpaces >> char '#' >> lineToEol

steps :: Parser Steps
steps = many $ try step

step :: Parser Step
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

examplesSection :: Parser ExamplesSection
examplesSection = many $ try examples

examples :: Parser Examples
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

multilineArg :: Parser MultilineArg
multilineArg = MAT <$> try table <|> MAPS <$> try pyString

pyString :: Parser PyString
pyString
  = openPyString >> strip <$> pack <$> manyTill anyChar (try closePyString)

openPyString :: Parser ()
openPyString = gSpaces >> string "\"\"\"" >> gSpaces >> eol

closePyString :: Parser ()
closePyString = eol >> gSpaces >> string "\"\"\"" >> white

cell :: Parser Cell
cell
  = do
    pCell <- many1 (noneOf "\r\n|")
    void (char '|')
    return $ strip $ pack pCell

row :: Parser Row
row
  = do
    white
    void $ char '|'
    pRow <- many1 $ try cell
    eol
    return pRow

table :: Parser Table
table = many1 $ try row

stepKeyword :: Parser StepKeyword
stepKeyword
  = read
    <$> choice
      (map (try . string . show) ([minBound .. maxBound] :: [StepKeyword]))

examplesKeyword :: Parser ()
examplesKeyword = void $ string "Examples:"

scenarioOutlineKeyword :: Parser ()
scenarioOutlineKeyword = void $ string "Scenario Outline:"

scenarioKeyword :: Parser ()
scenarioKeyword = void $ string "Scenario:"

backgroundKeyword :: Parser ()
backgroundKeyword = void $ string "Background:"

linesToKeyword :: Parser Text
linesToKeyword
  = strip
    <$> pack
    <$> manyTill
      anyChar
      (lookAhead $ try $ eol >> gSpaces >> reservedWordsAndSymbols)

reservedWordsAndSymbols :: Parser ()
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

gSpace :: Parser ()
gSpace = void $ oneOf " \t"

eol :: Parser ()
eol = optional (char '\r') >> void (char '\n')

white :: Parser ()
white = skipMany oneWhite

lineToEol :: Parser Text
lineToEol = pack <$> many (noneOf "\r\n")

keywordSpace :: Parser ()
keywordSpace = void $ char ' '

-- Not defined in BNF

oneWhite :: Parser ()
oneWhite = gSpace <|> eol

white1 :: Parser ()
white1 = skipMany1 oneWhite

gSpaces :: Parser ()
gSpaces = skipMany gSpace

scenarioCommon :: Parser (Text, Steps)
scenarioCommon
  = do
    gSpaces
    name <- linesToKeyword
    white
    pSteps <- steps
    return (name, pSteps)
