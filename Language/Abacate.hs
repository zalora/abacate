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

module
  Language.Abacate
  (module Language.Abacate.Types, parseFile,  parseAbacate)
  where

-- base
import Prelude hiding (readFile)
import Control.Applicative

-- text
import Data.Text hiding (concatMap)
import Data.Text.IO

-- parsec
import Text.Parsec

-- abacate
import Language.Abacate.Types
import Language.Abacate.BNF

parseFile :: FilePath -> IO (Either ParseError Abacate)
parseFile path = parseAbacateWithSource path <$> readFile path

parseAbacate :: Text -> Either ParseError Abacate
parseAbacate = parseAbacateWithSource "parseAbacate"

parseAbacateWithSource :: FilePath -> Text -> Either ParseError Abacate
parseAbacateWithSource = parse abacate

class GetSteps a where
  getSteps :: a -> Steps

instance GetSteps Feature where
  getSteps f
    = (case fBackground f of
        Nothing -> []
        Just bs -> getSteps bs)
      ++ concatMap getSteps (fFeatureElements f)

instance GetSteps FeatureElement where
  getSteps (FES s) = getSteps s
  getSteps (FESO so) = getSteps so

instance GetSteps Scenario where
  getSteps = getSteps . scBasicScenario

instance GetSteps ScenarioOutline where
  getSteps = getSteps . soScenario

instance GetSteps BasicScenario where
  getSteps = bsSteps
