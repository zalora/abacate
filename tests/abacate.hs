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

-- base
import System.Exit

-- text
import Data.Text hiding (map)

-- abacate
import Language.Abacate

-- HUnit
import Test.HUnit

main :: IO ()
main
  = do
    counts_ <- runTestTT tests
    if errors counts_ + failures counts_ > 0
      then exitFailure
      else exitSuccess

tests :: Test
tests
  = TestList
    [TestCase
        $ parseFile "tests/data/environment.feature"
          >>= (Feature
                empty
                []
                (pack
                  $ "Feature: Environment\n"
                    ++ "  In order to use the environment variables\n"
                    ++ "  Programmers must be able to write and read "
                    ++ "environment variables")
                Nothing
                [FES
                  $ Scenario []
                  $ BasicScenario
                    empty
                    (pack "Write then read")
                    [Step
                        empty
                        When
                        (pack "I set the variable as 3 into the environment")
                        Nothing,
                      Step
                        (pack " Comment! =)\n")
                        Then
                        (pack "the variable should have 3 on its content")
                        Nothing]]
                empty
              @=?)
            . fromRight,
      TestCase
        $ parseFile "tests/data/givenWhenGivenWhen.feature"
          >>= (Feature
                empty
                []
                (pack
                  $ "Feature: Given When Given When\n"
                    ++ "  In order to write more complicated test cases\n"
                    ++ "  Programmers must be able to write more than one "
                    ++ "Given When sequence")
                Nothing
                [FES
                  $ Scenario []
                  $ BasicScenario
                    empty
                    (pack "Given When Given When")
                    [Step
                        empty
                        Given
                        (pack "that A is logged in")
                        Nothing,
                      Step
                        empty
                        When
                        (pack "he goes to his user page")
                        Nothing,
                      Step
                        empty
                        Then
                        (pack "he sees his user name")
                        Nothing,
                      Step
                        empty
                        When
                        (pack "he clicks \"Premium\"")
                        Nothing,
                      Step
                        empty
                        Then
                        (pack "he sees that he is a premium user")
                        Nothing]]
                empty
              @=?)
            . fromRight,
      TestCase
        $ parseFile "tests/data/calculator.feature"
          >>= (Feature
                (pack " Of course it needs calc!\n")
                [pack "needs_calc", pack "nice_feature"]
                (pack
                  $ "Feature: Division\n"
                    ++ "  In order to avoid silly mistakes\n"
                    ++ "  Cashiers must be able to calculate a fraction")
                Nothing
                [FES
                  $ Scenario []
                  $ BasicScenario
                    empty
                    (pack "Regular numbers")
                    [Step
                        empty
                        Given
                        (pack "that I have entered 3 into the calculator")
                        Nothing,
                      Step
                        (pack " Comment! =)\n")
                        And
                        (pack "that I have entered 2 into the calculator")
                        Nothing,
                      Step
                        empty
                        When
                        (pack "I press divide")
                        Nothing,
                      Step
                        empty
                        Then
                        (pack "the result should be 1.5 on the screen")
                        Nothing]]
                empty
              @=?)
            . fromRight,
      TestCase
        $ parseFile "tests/data/wrongComment.feature"
          >>= (Feature
                empty
                []
                (pack
                  $ "Feature: Wrong Comment\n"
                    ++ "  In order to use # inside steps\n"
                    ++ "  Programmers must be able to write steps without "
                    ++ "being interpreted as comments")
                Nothing
                [FES
                  $ Scenario []
                  $ BasicScenario
                    empty
                    (pack "User logged in sees his page")
                    [Step
                        empty
                        Given
                        (pack "that #A is logged in")
                        Nothing,
                      Step
                        empty
                        When
                        (pack "he goes to his user page")
                        Nothing,
                      Step
                        empty
                        Then
                        (pack "he sees his user name")
                        Nothing]]
                empty
              @=?)
            . fromRight,
      TestCase
        $ parseFile "tests/data/complete.feature"
          >>= (Feature
                empty
                []
                (pack
                  $ "Feature: Complete\n"
                    ++ "  In order to use all the syntax\n"
                    ++ "  Users must be able to write all the syntax elements")
                (Just
                  $ BasicScenario
                    (pack " Background\n")
                    empty
                    [Step
                        empty
                        Given
                        (pack "a global administrator named \"Greg\"")
                        Nothing,
                      Step
                        empty
                        And
                        (pack "a blog named \"Greg's anti-tax rants\"")
                        Nothing,
                      Step
                        empty
                        And
                        (pack "a customer named \"Dr. Bill\"")
                        Nothing,
                      Step
                        empty
                        And
                        (pack
                          $ "a blog named \"Expensive Therapy\" owned by "
                            ++ "\"Dr. Bill\"")
                        Nothing])
                [FESO
                    $ ScenarioOutline
                      [Examples empty empty
                        $ map
                          (map pack)
                          [["start", "eat", "left"],
                            ["12", "5", "7"],
                            ["20", "5", "15"]]]
                    $ Scenario []
                    $ BasicScenario
                      (pack " Scenario outline\n")
                      (pack "eating")
                      [Step
                          empty
                          Given
                          (pack "there are <start> cucumbers")
                          Nothing,
                        Step
                          empty
                          When
                          (pack "I eat <eat> cucumbers")
                          Nothing,
                        Step
                          empty
                          Then
                          (pack "I should have <left> cucumbers")
                          Nothing],
                  FES
                    $ Scenario [pack "wip"]
                    $ BasicScenario
                      (pack " Scenario\n")
                      (pack
                        $ "Delete the blue lorry, which is magic and always "
                          ++ "raises an error")
                      [Step
                          empty
                          Given
                          (pack "the following lorries:")
                          (Just
                            $ MAT
                            $ map
                              (map pack)
                              [["name", "colour"],
                                ["name 1", "green"],
                                ["name 2", "yellow"],
                                ["name 3", "pink"],
                                ["name 4", "blue"]]),
                        Step
                          empty
                          But
                          (pack "I delete the 4th lorry")
                          Nothing,
                        Step
                          empty
                          Given
                          (pack
                             "a blog post named \"Random\" with Markdown body")
                          (Just
                            $ MAPS
                            $ pack
                            $ "Some Title, Eh?\n"
                              ++ "    ==============\n"
                              ++ "    Here is the first paragraph of my blog "
                              ++ "post. Lorem ipsum dolor sit amet,\n"
                              ++ "    consectetur adipiscing elit.")],
                  FESO
                    $ ScenarioOutline
                      [Examples empty empty
                        $ map
                          (map pack)
                          [["Role", "details"],
                            ["Manager",
                              "now able to manage your employee accounts"],
                            ["Admin",
                              "able to manage any user account on the system"]]]
                    $ Scenario []
                    $ BasicScenario
                      empty
                      (pack "Email confirmation")
                      [Step
                          empty
                          Given
                          (pack
                            $ "I have a user account with my name "
                              ++ "\"Jojo Binks\"")
                          Nothing,
                        Step
                          empty
                          When
                          (pack "an Admin grants me <Role> rights")
                          Nothing,
                        Step
                          empty
                          Then
                          (pack "I should receive an email with the body:")
                          (Just
                            $ MAPS
                            $ pack
                            $ "Dear Jojo Binks,\n"
                              ++ "      You have been granted <Role> rights.  "
                              ++ "You are <details>. Please be responsible.\n"
                              ++ "      -The Admins")],
                  FESO
                    $ ScenarioOutline
                      [Examples empty empty
                        $ map
                          (map pack)
                          [["x", "y"], ["2", "14"], ["10", "70"]]]
                    $ Scenario []
                    $ BasicScenario
                      empty
                      (pack "Multiplying with 7")
                      [Step
                          empty
                          Given
                          (pack "I have the numbers")
                          (Just
                            $ MAT
                            $ map
                              (map pack)
                              [["q1", "q2", "product"], ["<x>", "7", "<y>"]])]]
                (pack " Final comment\n")
              @=?)
            . fromRight]

fromRight :: Show a => Either a b -> b
fromRight (Right b) = b
fromRight (Left a) = error $ "fromRight (Left " ++ show a ++ ")"
