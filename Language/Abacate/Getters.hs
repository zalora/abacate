-- |
-- Module      :  Language.Abacate.Getters
-- Copyright   :  (c) Denis Shevchenko <me@dshevchenko.biz> 2015
-- License     :  Apache 2.0 (see the file LICENSE)
--
-- Maintainer  :  Denis Shevchenko <me@dshevchenko.biz>
-- Stability   :  unstable
-- Portability :  portable

module Language.Abacate.Getters where

import Language.Abacate.Types

import Data.Attoparsec.Text       
import Data.Char            (isDigit)
import Data.Text
import Data.Maybe

-- Extractors. More info about Abacate constructors, see:
-- http://hackage.haskell.org/package/abacate/docs/Language-Abacate-Types.html.

-- Steps from Background section.
doesBackgroundExist :: Abacate -> Bool
doesBackgroundExist = isJust . fBackground

backgroundSteps :: Abacate -> [Step]
backgroundSteps abacate = 
    if doesBackgroundExist abacate
        then bsSteps . fromJust . fBackground $ abacate
        else []

backgroundStep :: Abacate -> Int -> Step
backgroundStep abacate n = backgroundSteps abacate !! n

backgroundStepBody :: Abacate -> Int -> Text
backgroundStepBody abacate n = stBody $ backgroundStep abacate n

backgroundSingleStep :: Abacate -> Step
backgroundSingleStep abacate = abacate `backgroundStep` 0

backgroundSingleStepBody :: Abacate -> Text
backgroundSingleStepBody = stBody . backgroundSingleStep

-- Scenario sections.
isScenario :: FeatureElement -> Bool
isScenario (FES _)  = True 
isScenario (FESO _) = False 

isScenarioOutline :: FeatureElement -> Bool
isScenarioOutline = not . isScenario

scenarios :: Abacate -> [Scenario]
scenarios abacate =
    let only = [el | el <- fFeatureElements abacate, isScenario el]
    in [sc | FES sc <- only]

scenarioOutlines :: Abacate -> [ScenarioOutline]
scenarioOutlines abacate =
    let only = [el | el <- fFeatureElements abacate, isScenarioOutline el]    
    in [scO | FESO scO <- only]

-- Scenario.
scenario :: Abacate -> Int -> Scenario
scenario abacate n = scenarios abacate !! n

singleScenario :: Abacate -> Scenario
singleScenario abacate = abacate `scenario` 0

-- ScenarioOutline.
scenarioOutline :: Abacate -> Int -> ScenarioOutline
scenarioOutline abacate n = scenarioOutlines abacate !! n

singleScenarioOutline :: Abacate -> ScenarioOutline
singleScenarioOutline abacate = abacate `scenarioOutline` 0

-- Steps from Scenario section.
scenarioSteps :: Scenario -> [Step]
scenarioSteps = bsSteps . scBasicScenario

scenarioStep :: Scenario -> Int -> Step
scenarioStep aScenario n = scenarioSteps aScenario !! n

scenarioStepBody :: Scenario -> Int -> Text
scenarioStepBody aScenario n = stBody $ scenarioStep aScenario n

-- Work with ScenarioOutline section.
scenarioFromOutline :: ScenarioOutline -> Scenario
scenarioFromOutline = soScenario

examplesFromOutline :: ScenarioOutline -> [Examples]
examplesFromOutline = soExamplesSection

exampleFromOutline :: ScenarioOutline -> Int -> Examples
exampleFromOutline outline n = examplesFromOutline outline !! n

tableFromOutlineExample :: Examples -> Table
tableFromOutlineExample = eTable

-- Extractors for tables.
doesMultiLineExist :: Step -> Bool
doesMultiLineExist = isJust . stMultilineArg

multiLineArg :: Step -> MultilineArg
multiLineArg = fromJust . stMultilineArg

isTable :: MultilineArg -> Bool
isTable (MAT _)  = True 
isTable (MAPS _) = False 

isDocString :: MultilineArg -> Bool
isDocString = not . isTable

getTable :: MultilineArg -> Table
getTable arg = 
    let MAT table = arg in table

getDocString :: MultilineArg -> PyString
getDocString arg = 
    let MAPS docStr = arg in docStr

getTableFrom :: Step -> Table
getTableFrom step = 
    let MAT table = multiLineArg step in table

getDocStringFrom :: Step -> PyString
getDocStringFrom step = 
    let MAPS docStr = multiLineArg step in docStr

---------------- Useful parsers ----------------

-- Extracts single quoted string from a raw step.
getSingleQuotedStringFrom :: Text -> Text
getSingleQuotedStringFrom rawStep =
    case parseOnly manyQuotedStrings rawStep of
        Left  _    -> pack ""
        Right strs -> strs !! 0

-- Extracts all quoted strings from a raw step.
getQuotedStringsFrom :: Text -> [Text]
getQuotedStringsFrom rawStep =
    case parseOnly manyQuotedStrings rawStep of
        Left  _    -> []
        Right strs -> strs

-- Extracts single integer from a raw step.
getSingleIntegerFrom :: Text -> Maybe Integer
getSingleIntegerFrom rawStep =
    case parseOnly manyNumbers rawStep of
        Left  _       -> Nothing
        Right numbers -> Just (numbers !! 0)

-- Extracts all integers from a raw step.
getIntegersFrom :: Text -> [Integer]
getIntegersFrom rawStep =
    case parseOnly manyNumbers rawStep of
        Left  _       -> []
        Right numbers -> numbers

-- Attoparsec-based parsers.
manyNumbers :: Parser [Integer]
manyNumbers = many1 singleNumber
    where 
        singleNumber = do
            skipWhile (not . isDigit)
            d <- decimal
            skipWhile (not . isDigit)
            return d

manyQuotedStrings :: Parser [Text]
manyQuotedStrings = many1 singleQuotedString
    where 
        singleQuotedString = do
            skipWhile (/= '"')
            str <- char '"' *> manyTill' anyChar (char '"')
            return $ pack str
