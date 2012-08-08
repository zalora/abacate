Feature: Complete
  In order to use all the syntax
  Users must be able to write all the syntax elements

  # Background
  Background:
    Given a global administrator named "Greg"
    And a blog named "Greg's anti-tax rants"
    And a customer named "Dr. Bill"
    And a blog named "Expensive Therapy" owned by "Dr. Bill"

  # Scenario outline
  Scenario Outline: eating
    Given there are <start> cucumbers
    When I eat <eat> cucumbers
    Then I should have <left> cucumbers

    Examples:
      | start | eat | left |
      |  12   |  5  |  7   |
      |  20   |  5  |  15  |

  # Scenario
  @wip
  Scenario: Delete the blue lorry, which is magic and always raises an error
  Given the following lorries:
    | name | colour |
    | name 1 | green |
    | name 2 | yellow |
    | name 3 | pink |
    | name 4 | blue |
  But I delete the 4th lorry
  Given a blog post named "Random" with Markdown body
    """
    Some Title, Eh?
    ==============
    Here is the first paragraph of my blog post. Lorem ipsum dolor sit amet,
    consectetur adipiscing elit.
    """

  Scenario Outline: Email confirmation
    Given I have a user account with my name "Jojo Binks"
    When an Admin grants me <Role> rights
    Then I should receive an email with the body:
      """
      Dear Jojo Binks,
      You have been granted <Role> rights.  You are <details>. Please be responsible.
      -The Admins
      """
    Examples:
      |  Role     | details                                         |
      |  Manager  | now able to manage your employee accounts       |
      |  Admin    | able to manage any user account on the system   |

  Scenario Outline: Multiplying with 7
    Given I have the numbers
      | q1   | q2 | product |
      | <x>  |  7 | <y>     |
    Examples:
      |  x |  y |
      |  2 | 14 |
      | 10 | 70 |


# Final comment
