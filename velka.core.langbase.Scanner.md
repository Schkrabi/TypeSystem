# Scanner
Operators for working with java.util.Scanner.

## Table of Contents
* [constructor(construct Scanner Native)](#velka.core.langbase.Scanner$1)
* [close(scanner-native-close)](#velka.core.langbase.Scanner$2)
* [nextLine(scanner-native-next-line)](#velka.core.langbase.Scanner$3)
* [findInLine(scanner-native-find-in-line)](#velka.core.langbase.Scanner$5)
* [findWithinHorizon(scanner-native-find-within-horizon)](#velka.core.langbase.Scanner$6)
* [hasNext(scanner-native-has-next)](#velka.core.langbase.Scanner$7)
* [hasNextPattern(scanner-native-has-next-pattern)](#velka.core.langbase.Scanner$8)
* [hasNextBoolean(scanner-native-has-next-boolean)](#velka.core.langbase.Scanner$9)
* [hasNextDouble(scanner-native-has-next-double)](#velka.core.langbase.Scanner$10)
* [hasNextInt(scanner-native-has-next-int)](#velka.core.langbase.Scanner$11)
* [hasNextIntRadix(scanner-native-has-next-int-radix)](#velka.core.langbase.Scanner$12)
* [hasNextLine(scanner-native-has-next-line)](#velka.core.langbase.Scanner$13)
* [next(scanner-native-next)](#velka.core.langbase.Scanner$14)
* [nextPattern(scanner-native-next-pattern)](#velka.core.langbase.Scanner$15)
* [nextBool(scanner-native-next-bool)](#velka.core.langbase.Scanner$16)
* [nextDouble(scanner-native-next-double)](#velka.core.langbase.Scanner$17)
* [nextInt(scanner-native-next-int)](#velka.core.langbase.Scanner$18)
* [radix(scanner-native-radix)](#velka.core.langbase.Scanner$19)
* [reset(scanner-native-reset)](#velka.core.langbase.Scanner$20)
* [skip(scanner-native-skip)](#velka.core.langbase.Scanner$21)
* [useRadix(scanner-native-use-radix)](#velka.core.langbase.Scanner$23)

## Constructors
### <a name="velka.core.langbase.Scanner$1"> constructor(construct Scanner Native)</a>
Syntax:

~~~
(construct Scanner Native <filename>)
~~~

Type Signature:

~~~
[String:Native] -> Scanner:Native
~~~

Constructs Scanner:Native.

## Operators
### <a name="velka.core.langbase.Scanner$2"> close(scanner-native-close)</a>
Syntax:

~~~
(scanner-native-close <scanner>)
~~~

Type Signature:

~~~
[Scanner:Native] -> []
~~~

Closes scanner.

Example:

~~~
(scanner-native-close (construct Scanner Native "test-file"))
~~~
### <a name="velka.core.langbase.Scanner$3"> nextLine(scanner-native-next-line)</a>
Syntax:

~~~
(scanner-native-next-line <scanner>)
~~~

Type Signature:

~~~
[Scanner:Native] -> String:Native
~~~

Advances scanner past the current line and returns the input that was skipped.

Example:

~~~
(scanner-native-next-line (construct Scanner Native "test-file"))
~~~
### <a name="velka.core.langbase.Scanner$5"> findInLine(scanner-native-find-in-line)</a>
Syntax:

~~~
(scanner-native-find-in-line <scanner> <pattern>)
~~~

Type Signature:

~~~
[Scanner:Native, String:Native] -> String:Native
~~~

Attempts to find the next occurrence of a pattern constructed from the specified string, ignoring delimiters.

Example:

~~~
(scanner-native-find-in-line (construct Scanner Native "test-file") "a*b")
~~~
### <a name="velka.core.langbase.Scanner$6"> findWithinHorizon(scanner-native-find-within-horizon)</a>
Syntax:

~~~
(scanner-native-find-within-horizon <scanner> <pattern> <horizon>)
~~~

Type Signature:

~~~
[Scanner:Native, String:Native, Int:Native] -> String:Native
~~~

Attempts to find the next occurrence of a pattern constructed from the specified string, ignoring delimiters.

Example:

~~~
(scanner-native-find-within-horizon (construct Scanner Native "test-file") "a*b" 42)
~~~
### <a name="velka.core.langbase.Scanner$7"> hasNext(scanner-native-has-next)</a>
Syntax:

~~~
(scanner-native-find-within-horizon <scanner>)
~~~

Type Signature:

~~~
[Scanner:Native] -> Bool:Native
~~~

Attempts to find the next occurrence of a pattern constructed from the specified string, ignoring delimiters.

Example:

~~~
(scanner-native-find-within-horizon (construct Scanner Native "test-file") "a*b" 42)
~~~
### <a name="velka.core.langbase.Scanner$8"> hasNextPattern(scanner-native-has-next-pattern)</a>
Syntax:

~~~
(scanner-native-has-next-pattern <scanner> <pattern>)
~~~

Type Signature:

~~~
[Scanner:Native, String:Native] -> Bool:Native
~~~

Returns true if the next token matches the pattern constructed from the specified string.

Example:

~~~
(scanner-native-has-next-pattern (construct Scanner Native "test-file") "a*b")
~~~
### <a name="velka.core.langbase.Scanner$9"> hasNextBoolean(scanner-native-has-next-boolean)</a>
Syntax:

~~~
(scanner-native-has-next-boolean <scanner>)
~~~

Type Signature:

~~~
[Scanner:Native] -> Bool:Native
~~~

Returns true if the next token in this scanner's input can be interpreted as a boolean value using a case insensitive pattern created from the string "true|false".

Example:

~~~
(scanner-native-has-next-boolean (construct Scanner Native "test-file"))
~~~
### <a name="velka.core.langbase.Scanner$10"> hasNextDouble(scanner-native-has-next-double)</a>
Syntax:

~~~
(scanner-native-has-next-double <scanner>)
~~~

Type Signature:

~~~
[Scanner:Native] -> Bool:Native
~~~

Returns true if the next token in this scanner's input can be interpreted as a double value using the scanner-native-next-double function.

Example:

~~~
(scanner-native-has-next-double (construct Scanner Native "test-file"))
~~~
### <a name="velka.core.langbase.Scanner$11"> hasNextInt(scanner-native-has-next-int)</a>
Syntax:

~~~
(scanner-native-has-next-int <scanner>)
~~~

Type Signature:

~~~
[Scanner:Native] -> Bool:Native
~~~

Returns true if the next token in this scanner's input can be interpreted as an int value in the default radix using the scanner-native-next-int function.

Example:

~~~
(scanner-native-has-next-int (construct Scanner Native "test-file"))
~~~
### <a name="velka.core.langbase.Scanner$12"> hasNextIntRadix(scanner-native-has-next-int-radix)</a>
Syntax:

~~~
(scanner-native-has-next-int-radix <scanner> <radix>)
~~~

Type Signature:

~~~
[Scanner:Native, Int:Native] -> Bool:Native
~~~

Returns true if the next token in this scanner's input can be interpreted as an int value in the specified radix using the next-int-radix function.

Example:

~~~
(scanner-native-has-next-int-radix (construct Scanner Native "test-file" 16))
~~~
### <a name="velka.core.langbase.Scanner$13"> hasNextLine(scanner-native-has-next-line)</a>
Syntax:

~~~
(scanner-native-has-next-line <scanner>)
~~~

Type Signature:

~~~
[Scanner:Native] -> Bool:Native
~~~

Returns true if there is another line in the input of this scanner.

Example:

~~~
(scanner-native-has-next-line (construct Scanner Native "test-file"))
~~~
### <a name="velka.core.langbase.Scanner$14"> next(scanner-native-next)</a>
Syntax:

~~~
(scanner-native-next <scanner>)
~~~

Type Signature:

~~~
[Scanner:Native] -> String:Native
~~~

Finds and returns the next complete token from this scanner.

Example:

~~~
(scanner-native-next (construct Scanner Native "test-file"))
~~~
### <a name="velka.core.langbase.Scanner$15"> nextPattern(scanner-native-next-pattern)</a>
Syntax:

~~~
(scanner-native-next-pattern <scanner> <pattern>)
~~~

Type Signature:

~~~
[Scanner:Native, String:Native] -> String:Native
~~~

Returns the next token if it matches the pattern constructed from the specified string.

Example:

~~~
(scanner-native-next-pattern (construct Scanner Native "test-file") "a*b")
~~~
### <a name="velka.core.langbase.Scanner$16"> nextBool(scanner-native-next-bool)</a>
Syntax:

~~~
(scanner-native-next-bool <scanner>)
~~~

Type Signature:

~~~
[Scanner:Native] -> Bool:Native
~~~

Scans the next token of the input into a boolean value and returns that value.

Example:

~~~
(scanner-native-next-bool (construct Scanner Native "test-file"))
~~~
### <a name="velka.core.langbase.Scanner$17"> nextDouble(scanner-native-next-double)</a>
Syntax:

~~~
(scanner-native-next-double <scanner>)
~~~

Type Signature:

~~~
[Scanner:Native] -> Double:Native
~~~

Scans the next token of the input as a double.

Example:

~~~
(scanner-native-next-double (construct Scanner Native "test-file"))
~~~
### <a name="velka.core.langbase.Scanner$18"> nextInt(scanner-native-next-int)</a>
Syntax:

~~~
(scanner-native-next-int <scanner>)
~~~

Type Signature:

~~~
[Scanner:Native] -> Int:Native
~~~

Scans the next token of the input as an int.

Example:

~~~
(scanner-native-next-int (construct Scanner Native "test-file"))
~~~
### <a name="velka.core.langbase.Scanner$19"> radix(scanner-native-radix)</a>
Syntax:

~~~
(scanner-native-radix <scanner>)
~~~

Type Signature:

~~~
[Scanner:Native] -> Int:Native
~~~

Returns this scanner's default radix.

Example:

~~~
(scanner-native-radix (construct Scanner Native "test-file"))
~~~
### <a name="velka.core.langbase.Scanner$20"> reset(scanner-native-reset)</a>
Syntax:

~~~
(scanner-native-reset <scanner>)
~~~

Type Signature:

~~~
[Scanner:Native] -> Scanner:Native
~~~

Resets this scanner.

Example:

~~~
(scanner-native-reset (construct Scanner Native "test-file"))
~~~
### <a name="velka.core.langbase.Scanner$21"> skip(scanner-native-skip)</a>
Syntax:

~~~
(scanner-native-skip <scanner> <pattern>)
~~~

Type Signature:

~~~
[Scanner:Native, String:Native] -> Scanner:Native
~~~

Skips input that matches a pattern constructed from the specified string.

Example:

~~~
(scanner-native-skip (construct Scanner Native "test-file") "a*b")
~~~
### <a name="velka.core.langbase.Scanner$23"> useRadix(scanner-native-use-radix)</a>
Syntax:

~~~
(scanner-native-use-radix <scanner> <delimiter>)
~~~

Type Signature:

~~~
[Scanner:Native, Int:Native] -> Scanner:Native
~~~

Sets this scanner's default radix to the specified radix.

Example:

~~~
(scanner-native-use-radix (construct Scanner Native "test-file") "a")
~~~