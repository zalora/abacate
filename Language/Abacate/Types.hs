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

module Language.Abacate.Types where

-- text
import Data.Text

type Abacate = Feature

data Feature
  = Feature
      {gBegComment :: Comment,
        gTags :: Tags,
        gHeader :: Header,
        gBackground :: Maybe Background,
        gFeatureElements :: FeatureElements,
        gEndComment :: Comment}
    deriving (Eq, Show)

type Header = Text
type FeatureElements = [FeatureElement]

data FeatureElement
  = FES Scenario | FESO ScenarioOutline
    deriving (Eq, Show)

data BasicScenario
  = BasicScenario {bsComment :: Comment, bsName :: Text, bsSteps :: Steps}
    deriving (Eq, Show)

data Scenario
  = Scenario {scTags :: Tags, scBasicScenario :: BasicScenario}
    deriving (Eq, Show)

data ScenarioOutline
  = ScenarioOutline
      {soExamplesSection :: ExamplesSection, soScenario :: Scenario}
    deriving (Eq, Show)

type Background = BasicScenario
type Tags = [Tag]
type Tag = Text
type Comment = Text
type Steps = [Step]

data Step
  = Step
      {stComment :: Comment,
        stStepKeyword :: StepKeyword,
        stBody :: Text,
        stMultilineArg :: Maybe MultilineArg}
    deriving (Eq, Show)

type ExamplesSection = [Examples]

data Examples
  = Examples {eComment :: Comment, eName :: Text, eTable :: Table}
    deriving (Eq, Show)

data MultilineArg = MAT Table | MAPS PyString deriving (Eq, Show)
type PyString = Text
type Table = [Row]
type Row = [Cell]
type Cell = Text

data StepKeyword
  = Given | When | Then | And | But deriving (Eq, Bounded, Enum, Show, Read)

