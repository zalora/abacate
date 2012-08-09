-- |
-- Module      :  Language.Abacate.Types
-- Copyright   :  (c) Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com> 2012
-- License     :  Apache 2.0 (see the file LICENSE)
--
-- Maintainer  :  Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
-- Stability   :  unstable
-- Portability :  portable
module Language.Abacate.Types where

-- text
import Data.Text

-- | An Abacate file contains exactly one feature.
type Abacate = Feature

-- | For a more detailed description of features, check
-- <https://github.com/cucumber/cucumber/wiki/Feature-Introduction>.
data Feature
  = Feature
      {fBegComment :: Comment,
        fTags :: Tags,
        fHeader :: Header,
        fBackground :: Maybe Background,
        fFeatureElements :: FeatureElements,
        fEndComment :: Comment}
    deriving (Eq, Show)

-- | The 'Header' contains the title and description of the 'Feature'.  It is
-- usually on the format:
--
-- @
-- Feature: TITLE
--   DESCRIPTION LINE 1
--   DESCRIPTION LINE 2
-- @
type Header = Text

type FeatureElements = [FeatureElement]

-- | A 'FeatureElement' is simply a 'Scenario' or a 'ScenarioOutline'.
data FeatureElement
  = FES Scenario | FESO ScenarioOutline
    deriving (Eq, Show)

-- | The 'Scenario' contains a list of 'Steps'.
data Scenario
  = Scenario {scTags :: Tags, scBasicScenario :: BasicScenario}
    deriving (Eq, Show)

-- | The 'Steps' of the 'ScenarioOutline' can contain placeholders which are
-- substituted by the fields of the 'ExamplesSection'.  For more information on
-- the 'ScenarioOutline' check
-- <https://github.com/cucumber/cucumber/wiki/Scenario-outlines>.
data ScenarioOutline
  = ScenarioOutline
      {soExamplesSection :: ExamplesSection, soScenario :: Scenario}
    deriving (Eq, Show)

-- | The 'Background' is basically a 'Scenario' without 'Tags'.  It's used to
-- create a context for the other 'Scenario's to run, and a 'Feature' can only
-- have one 'Background'.  For more information on the 'Background' check
-- <https://github.com/cucumber/cucumber/wiki/Background>.
type Background = BasicScenario

-- | This is the basic type for 'Scenario', 'ScenarioOutline' and 'Background'.
data BasicScenario
  = BasicScenario {bsComment :: Comment, bsName :: Text, bsSteps :: Steps}
    deriving (Eq, Show)

type Tags = [Tag]

-- | A 'Tag' is a textual element started with \@.
type Tag = Text

type Comment = Text
type Steps = [Step]

-- | For more information on 'Steps' check
-- <https://github.com/cucumber/cucumber/wiki/Given-When-Then>.
data Step
  = Step
      {stComment :: Comment,
        stStepKeyword :: StepKeyword,
        stBody :: Text,
        stMultilineArg :: Maybe MultilineArg}
    deriving (Eq, Show)

type ExamplesSection = [Examples]

-- | The 'Examples' are used to fill the value of placeholders on
-- 'ScenarioOutline's.
data Examples
  = Examples {eComment :: Comment, eName :: Text, eTable :: Table}
    deriving (Eq, Show)

-- | 'Steps' can have multi-line arguments.  For more information check
-- <https://github.com/cucumber/cucumber/wiki/Multiline-Step-Arguments>.
data MultilineArg = MAT Table | MAPS PyString deriving (Eq, Show)

type PyString = Text
type Table = [Row]
type Row = [Cell]
type Cell = Text

data StepKeyword
  = Given | When | Then | And | But deriving (Eq, Bounded, Enum, Show, Read)
