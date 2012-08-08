Feature: Wrong Comment
  In order to use # inside steps
  Programmers must be able to write steps without being interpreted as comments

  Scenario: User logged in sees his page
    Given that #A is logged in
    When he goes to his user page
    Then he sees his user name
