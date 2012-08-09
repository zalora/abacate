-- |
-- Module      :  Language.Abacate
-- Copyright   :  (c) Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com> 2012
-- License     :  Apache 2.0 (see the file LICENSE)
--
-- Maintainer  :  Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
-- Stability   :  unstable
-- Portability :  portable
module
  Language.Abacate
  (module Language.Abacate.Types, parseFile,  parseAbacate)
  where

-- base
import Prelude hiding (readFile)
import Control.Applicative

-- text
import Data.Text
import Data.Text.IO

-- parsec
import Text.Parsec

-- abacate
import Language.Abacate.Types
import Language.Abacate.BNF

-- | Parses a file with 'Abacate' code.
parseFile :: FilePath -> IO (Either ParseError Abacate)
parseFile path = parseAbacateWithSource path <$> readFile path

-- | Parses a string with 'Abacate' code.
parseAbacate :: Text -> Either ParseError Abacate
parseAbacate = parseAbacateWithSource "parseAbacate"

parseAbacateWithSource :: FilePath -> Text -> Either ParseError Abacate
parseAbacateWithSource = parse abacate