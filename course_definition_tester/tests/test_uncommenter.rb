require "minitest/autorun"
require "minitest/color"

require_relative "../lib/uncommenter"

SAMPLE_TWO_MARKERS_COMMENTED = """
a = b

# Uncomment this to pass the first stage
#
# # First uncommented block
# b = c

# Uncomment this to pass the first stage
#
# # Second uncommented block
# c = d
"""

SAMPLE_TWO_MARKERS_UNCOMMENTED = """
a = b

# Uncomment this to pass the first stage

# First uncommented block
b = c

# Uncomment this to pass the first stage

# Second uncommented block
c = d
"""

SAMPLE_PY_COMMENTED = """
abcd = true

# Uncomment this to pass the first stage
#
# # This is an assignment
# a = b
#
# if True:
#     pass
#
# blah

yay = true
"""

SAMPLE_PY_UNCOMMENTED = """
abcd = true

# Uncomment this to pass the first stage

# This is an assignment
a = b

if True:
    pass

blah

yay = true
"""

SAMPLE_GO_COMMENTED = """
func main() {
  // Uncomment this to pass the first stage
  //
  // // This is an assignment
  // a := 1
  //
  // fmt.Println('hey')

  a := 2
}
"""

SAMPLE_GO_UNCOMMENTED = """
func main() {
  // Uncomment this to pass the first stage

  // This is an assignment
  a := 1

  fmt.Println('hey')

  a := 2
}
"""

SAMPLE_HASKELL_COMMENTD = """
main = do
 -- Uncomment this to pass the first stage
 -- a <- readLine
 -- b <- readLine
 -- -- Nested Comment
 -- return (a + b)
"""


SAMPLE_HASKELL_UNCOMMENTD = """
main = do
 -- Uncomment this to pass the first stage
 a <- readLine
 b <- readLine
 -- Nested Comment
 return (a + b)
"""

SAMPLE_JAVA_COMMENTED = """
public static void main(String[] args) {
  // Uncomment this to pass the first stage
  //
  // // This is an assignment
  // int a = 1;
  //
  // System.out.println('Hey');

  int b = 2;
}
"""

SAMPLE_JAVA_UNCOMMENTED = """
public static void main(String[] args) {
  // Uncomment this to pass the first stage

  // This is an assignment
  int a = 1;

  System.out.println('Hey');

  int b = 2;
}
"""

SAMPLE_PHP_COMMENTED = """
<?php
// Uncomment this to pass the first stage.
// $a = 1;
// $b = 1;

// echo $a + $b;
?>
"""

SAMPLE_PHP_UNCOMMENTED = """
<?php
// Uncomment this to pass the first stage.
$a = 1;
$b = 1;

// echo $a + $b;
?>
"""

SAMPLE_JAVASCRIPT_COMMENTED = """
// Uncomment this to pass the first stage
// var a = 1;
// var b = 2;
// console.log(a + b);
"""

SAMPLE_JAVASCRIPT_UNCOMMENTED = """
// Uncomment this to pass the first stage
var a = 1;
var b = 2;
console.log(a + b);
"""

SAMPLE_CSHARP_COMMENTED = """
// Uncomment this to pass the first stage
// var a = 1;
// var b = 2;
// Console.WriteLine(a + b);
"""

SAMPLE_CSHARP_UNCOMMENTED = """
// Uncomment this to pass the first stage
var a = 1;
var b = 2;
Console.WriteLine(a + b);
"""

UNCOMMENT_PATTERN = /Uncomment this/

class TestUncommenter < Minitest::Test
  def test_python
    actual = Uncommenter.new("python", SAMPLE_PY_COMMENTED, UNCOMMENT_PATTERN).uncommented
    expected = SAMPLE_PY_UNCOMMENTED
    assert_equal expected, actual
  end

  def test_noop_if_no_uncomment_marker
    assert_equal SAMPLE_PY_COMMENTED, Uncommenter.new("python", SAMPLE_PY_COMMENTED, /not found/).uncommented
  end

  def test_twice_python
    actual = Uncommenter.new("python", SAMPLE_PY_UNCOMMENTED, UNCOMMENT_PATTERN).uncommented
    expected = SAMPLE_PY_UNCOMMENTED
    assert_equal expected, actual
  end

  def test_go
    actual = Uncommenter.new("go", SAMPLE_GO_COMMENTED, UNCOMMENT_PATTERN).uncommented
    expected = SAMPLE_GO_UNCOMMENTED
    assert_equal expected, actual
  end

  def test_twice_go
    actual = Uncommenter.new("go", SAMPLE_GO_UNCOMMENTED, UNCOMMENT_PATTERN).uncommented
    expected = SAMPLE_GO_UNCOMMENTED
    assert_equal expected, actual
  end

  def test_haskell
    actual = Uncommenter.new("haskell", SAMPLE_HASKELL_COMMENTD, UNCOMMENT_PATTERN).uncommented
    expected = SAMPLE_HASKELL_UNCOMMENTD
    assert_equal expected, actual
  end

  def test_twice_haskell
    actual = Uncommenter.new("haskell", SAMPLE_HASKELL_UNCOMMENTD, UNCOMMENT_PATTERN).uncommented
    expected = SAMPLE_HASKELL_UNCOMMENTD
    assert_equal expected, actual
  end

  def test_java
    actual = Uncommenter.new("java", SAMPLE_JAVA_COMMENTED, UNCOMMENT_PATTERN).uncommented
    expected = SAMPLE_JAVA_UNCOMMENTED
    assert_equal expected, actual
  end

  def test_twice_java
    actual = Uncommenter.new("java", SAMPLE_JAVA_UNCOMMENTED, UNCOMMENT_PATTERN).uncommented
    expected = SAMPLE_JAVA_UNCOMMENTED
    assert_equal expected, actual
  end

  def test_php
    actual = Uncommenter.new("php", SAMPLE_PHP_COMMENTED, UNCOMMENT_PATTERN).uncommented
    expected = SAMPLE_PHP_UNCOMMENTED
    assert_equal expected, actual
  end

  def test_twice_php
    actual = Uncommenter.new("php", SAMPLE_PHP_UNCOMMENTED, UNCOMMENT_PATTERN).uncommented
    expected = SAMPLE_PHP_UNCOMMENTED
    assert_equal expected, actual
  end

  def test_javascript
    actual = Uncommenter.new("javascript", SAMPLE_JAVASCRIPT_COMMENTED, UNCOMMENT_PATTERN).uncommented
    expected = SAMPLE_JAVASCRIPT_UNCOMMENTED
    assert_equal expected, actual
  end

  def test_twice_javascript
    actual = Uncommenter.new("javascript", SAMPLE_JAVASCRIPT_UNCOMMENTED, UNCOMMENT_PATTERN).uncommented
    expected = SAMPLE_JAVASCRIPT_UNCOMMENTED
    assert_equal expected, actual
  end

  def test_csharp
    actual = Uncommenter.new("csharp", SAMPLE_CSHARP_COMMENTED, UNCOMMENT_PATTERN).uncommented
    expected = SAMPLE_CSHARP_UNCOMMENTED
    assert_equal expected, actual
  end

  def test_twice_csharp
    actual = Uncommenter.new("csharp", SAMPLE_CSHARP_UNCOMMENTED, UNCOMMENT_PATTERN).uncommented
    expected = SAMPLE_CSHARP_UNCOMMENTED
    assert_equal expected, actual
  end

  def test_two_markers
    actual = Uncommenter.new("python", SAMPLE_TWO_MARKERS_COMMENTED, UNCOMMENT_PATTERN).uncommented
    expected = SAMPLE_TWO_MARKERS_UNCOMMENTED
    assert_equal expected, actual
  end
end
