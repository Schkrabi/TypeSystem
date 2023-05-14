# General
General operators for primitive types and utility.

## Table of Contents
* [Addition(+)](#velka.core.langbase.Operators$1)
* [BitAnd(bit-and)](#velka.core.langbase.Operators$2)
* [BitNot(bit-not)](#velka.core.langbase.Operators$3)
* [BitOr(bit-or)](#velka.core.langbase.Operators$4)
* [BitShiftLeft(shl)](#velka.core.langbase.Operators$5)
* [BitShiftRight(shr)](#velka.core.langbase.Operators$6)
* [BitXor(bit-xor)](#velka.core.langbase.Operators$7)
* [CanUnifyRepresentations(can-unify-representations)](#velka.core.langbase.Operators$8)
* [CanUnifyTypes(can-unify-types)](#velka.core.langbase.Operators$9)
* [Car(car)](#velka.core.langbase.Operators$10)
* [Cdr(cdr)](#velka.core.langbase.Operators$11)
* [Concantenation(concat)](#velka.core.langbase.Operators$12)
* [ConversionCost(conversion-cost)](#velka.core.langbase.Operators$13)
* [Division(/)](#velka.core.langbase.Operators$14)
* [DoubleAddition(dadd)](#velka.core.langbase.Operators$15)
* [DoubleDivision(ddiv)](#velka.core.langbase.Operators$16)
* [DoubleLesserThan(dlt)](#velka.core.langbase.Operators$17)
* [Equals(equals?)](#velka.core.langbase.Operators$18)
* [InitLogger(init-logger)](#velka.core.langbase.Operators$19)
* [IntToDouble(int-to-double)](#velka.core.langbase.Operators$20)
* [Floor(floor)](#velka.core.langbase.Operators$21)
* [IsSameRepresentation(is-same-representation)](#velka.core.langbase.Operators$22)
* [IsSameType(is-same-type)](#velka.core.langbase.Operators$23)
* [LesserThan(<)](#velka.core.langbase.Operators$24)
* [Log(log)](#velka.core.langbase.Operators$25)
* [Modulo(mod)](#velka.core.langbase.Operators$26)
* [Multiplication(*)](#velka.core.langbase.Operators$27)
* [Not(not)](#velka.core.langbase.Operators$28)
* [NumericEqual(=)](#velka.core.langbase.Operators$29)
* [ParseInt(parse-int)](#velka.core.langbase.Operators$30)
* [PrintlnOperator(println)](#velka.core.langbase.Operators$31)
* [ReadFile(read-file)](#velka.core.langbase.Operators$32)
* [StrSplit(str-split)](#velka.core.langbase.Operators$33)
* [Subtraction(-)](#velka.core.langbase.Operators$34)
* [Timestamp(timestamp)](#velka.core.langbase.Operators$35)
* [ToStr(to-str)](#velka.core.langbase.Operators$36)
* [UnsignedBitShiftRight(ushr)](#velka.core.langbase.Operators$37)
* [typeStr(type-str)](#velka.core.langbase.Operators$38)
* [representationStr(representation-str)](#velka.core.langbase.Operators$39)
* [substr(substr)](#velka.core.langbase.Operators$40)
* [strlen(strlen)](#velka.core.langbase.Operators$41)



## Operators
### <a name="velka.core.langbase.Operators$1"> Addition(+)</a>
Syntax:

~~~
(+ arg1 arg2)
~~~

Type Signature:

~~~
[Int:Native, Int:Native] -> Int:Native
~~~

Adds two integers.

Example:

~~~
(+ 21 21) ; = 42
~~~
### <a name="velka.core.langbase.Operators$2"> BitAnd(bit-and)</a>
Syntax:

~~~
(bit-and <arg1> <arg2>)
~~~

Type Signature:

~~~
[Int:Native, Int:Native] -> Int:Native
~~~

Performs bit-wise and of two integers.

Example:

~~~
(bit-and 5 1) ; = 1
~~~
### <a name="velka.core.langbase.Operators$3"> BitNot(bit-not)</a>
Syntax:

~~~
(bit-not <arg>)
~~~

Type Signature:

~~~
[Int:Native] -> Int:Native
~~~

Negates all bits in binary representation of the argument.

Example:

~~~
>(bit-not 42) ;;= -43
~~~
### <a name="velka.core.langbase.Operators$4"> BitOr(bit-or)</a>
Syntax:

~~~
(bit-or <arg1> <arg2>)
~~~

Type Signature:

~~~
[Int:Native, Int:Native] -> Int:Native
~~~

Performs bit-wise or of two integers.

Example:

~~~
(bit-or 5 1) ; = 5
~~~
### <a name="velka.core.langbase.Operators$5"> BitShiftLeft(shl)</a>
Syntax:

~~~
(shl <bits> <n>)
~~~

Type Signature:

~~~
[Int:Native, Int:Native] -> Int:Native
~~~

Shifts bits of binary representation_bits_ left by _n_ positions.

Example:

~~~
>(shl 1 4) ;;=16
~~~
### <a name="velka.core.langbase.Operators$6"> BitShiftRight(shr)</a>
Syntax:

~~~
(shr <bits> <n>)
~~~

Type Signature:

~~~
[Int:Native, Int:Native] -> Int:Native
~~~

Shifts bits of binary representation_bits_ right by _n_ positions.

Example:

~~~
>(shr 16 4) ;; = 1
~~~
### <a name="velka.core.langbase.Operators$7"> BitXor(bit-xor)</a>
Syntax:

~~~
(bit-xor <arg1> <arg2>)
~~~

Type Signature:

~~~
[Int:Native, Int:Native] -> Int:Native
~~~

Computes xor of binary representations of integer arguments.

Example:

~~~
>(bit-xor 6 3) ;; = 5
~~~
### <a name="velka.core.langbase.Operators$8"> CanUnifyRepresentations(can-unify-representations)</a>
Syntax:

~~~
(can-unify-representations <arg1> <arg2>)
~~~

Type Signature:

~~~
[SYSGENNAMEsm, SYSGENNAMEsn] -> Bool:Native
~~~

Returns _true_ if representations carried by _arg1_ and _arg2_ can be unified. Otherwise returns _false_. This operator works on level of representations, meaning for example _Int:Native_ and _Int:String_ cannot unify and type symbols carriing them will yield _false_ on application of the operator.

For type level variant see [can-unify-types](#canUnifyTypes).

Example:

~~~
(can-unify-representations 42 42) ;; = #t
(can-unify-representations 42 (construct Int String "42"))
~~~
### <a name="velka.core.langbase.Operators$9"> CanUnifyTypes(can-unify-types)</a>
Syntax:

~~~
(can-unify-types <arg1> <arg2>)
~~~

Type Signature:

~~~
[SYSGENNAMEtm, SYSGENNAMEtn] -> Bool:Native
~~~

Returns _true_ if types carried by _arg1_ and _arg2_ can be unified. Otherwise returns _false_. This operator works on level of types, not taking type representations into account. Meaning for example _Int:Native_ and _Int:String_ can unify and type symbols carriing them will yield _true_ on application of the operator. 

For representation level variant see [can-unify-representations](#canUnifyRepresentations).

Example:

~~~
(can-unify-types 42 (construct Int String "42")) ;; = #t)
(can-unify-types 42 "42") ;; = #f
~~~
### <a name="velka.core.langbase.Operators$10"> Car(car)</a>
Syntax:

~~~
(car <arg>)
~~~

Type Signature:

~~~
[[SYSGENNAMEum, SYSGENNAMEun]] -> SYSGENNAMEum
~~~

Extracts first value from a pair.

Example:

~~~
(car (cons 42 "42")) ; = 42
~~~
### <a name="velka.core.langbase.Operators$11"> Cdr(cdr)</a>
Syntax:

~~~
(cdr <arg>)
~~~

Type Signature:

~~~
[[SYSGENNAMEvm, SYSGENNAMEvn]] -> SYSGENNAMEvn
~~~

Extracts second value from a pair.

Example:

~~~
(cdr (cons 42 "42")) ; = "42"
~~~
### <a name="velka.core.langbase.Operators$12"> Concantenation(concat)</a>
Syntax:

~~~
(concat <arg1> <arg2>)
~~~

Type Signature:

~~~
[String:Native, String:Native] -> String:Native
~~~

Concatenates two strings.

Example:

~~~
(concat "foo" "bar") ; = "foobar"
~~~
### <a name="velka.core.langbase.Operators$13"> ConversionCost(conversion-cost)</a>
Syntax:

~~~
(conversion-cost
	(lambda ((Int:Native x) (Int:Native y)) (+ x y))
	(tuple
		(construct Int String "42")
		(construct Int Roman "XLII"))) ; = 2
~~~

Type Signature:

~~~
[SYSGENNAMExk -> SYSGENNAMExl, SYSGENNAMExk] -> Int:Native
~~~

Computes cost of representation conversion for applying _fun_ with _arg_.

Example:

~~~
(conversion-cost <fun> <arg>)
~~~
### <a name="velka.core.langbase.Operators$14"> Division(/)</a>
Syntax:

~~~
(/ <arg1> <arg2)
~~~

Type Signature:

~~~
[Int:Native, Int:Native] -> Int:Native
~~~

Divides _arg1_ by _arg2_. if _arg2_ evaluates to zero, causes exception.

Example:

~~~
(/ 84 2) ; = 42
~~~
### <a name="velka.core.langbase.Operators$15"> DoubleAddition(dadd)</a>
Syntax:

~~~
(dadd <arg1> <arg2>)
~~~

Type Signature:

~~~
[Double:Native, Double:Native] -> Double:Native
~~~

Adds two double numbers.

Example:

~~~
(dadd 21.5 22.5) += 42.0
~~~
### <a name="velka.core.langbase.Operators$16"> DoubleDivision(ddiv)</a>
Syntax:

~~~
(ddiv <arg1> <arg2>)
~~~

Type Signature:

~~~
[Double:Native, Double:Native] -> Double:Native
~~~

Divides _arg1_ by _arg2_. If _arg2_ evaluates to zero, causes exception.

Example:

~~~
(ddiv 8.4 0.2) ;= 42.0
~~~
### <a name="velka.core.langbase.Operators$17"> DoubleLesserThan(dlt)</a>
Syntax:

~~~
(dlt <arg1> <arg2>)
~~~

Type Signature:

~~~
[Double:Native, Double:Native] -> Bool:Native
~~~

Comapres two doubles. Returns _true_  if _arg1_ is smaller or equal than _arg2_, otherwise returns _false_.

Example:

~~~
(dlt 42.1 54.3) ; = #t
(dlt 42.1 21.3) + = #f
~~~
### <a name="velka.core.langbase.Operators$18"> Equals(equals?)</a>
Syntax:

~~~
(equals? <arg1> <arg2>)
~~~

Type Signature:

~~~
[SYSGENNAMEa, SYSGENNAMEb] -> Bool:Native
~~~

Returns true if arguments are equal, otherwise returns false.

Example:

~~~
(equals? 42 "42") ; = #f
(equals? (cons 42 42) (cons 42 42)) ; = #t
~~~
### <a name="velka.core.langbase.Operators$19"> InitLogger(init-logger)</a>
Syntax:

~~~
(init-logger <name>)
~~~

Type Signature:

~~~
[String:Native] -> []
~~~

Initializes logger, which will write to file specified by _name_. For logging messages see [log](#log).

Example:

~~~
>(init-logger "test-log.log")
[]
~~~
### <a name="velka.core.langbase.Operators$20"> IntToDouble(int-to-double)</a>
Syntax:

~~~
(int-to-double <arg>)
~~~

Type Signature:

~~~
[Int:Native] -> Double:Native
~~~

Coerces _arg_ to _Double:Native_ type.

Example:

~~~
(int-to-double 42) ; = 42.0
~~~
### <a name="velka.core.langbase.Operators$21"> Floor(floor)</a>
Syntax:

~~~
(int-to-double <arg>)
~~~

Type Signature:

~~~
[Double:Native] -> Int:Native
~~~

Coerces _arg_ to _Double:Native_ type.

Example:

~~~
(int-to-double 42) ; = 42.0
~~~
### <a name="velka.core.langbase.Operators$22"> IsSameRepresentation(is-same-representation)</a>
Syntax:

~~~
(is-same-representation <arg1> <arg2>)
~~~

Type Signature:

~~~
[SYSGENNAMEafu, SYSGENNAMEafv] -> Bool:Native
~~~

Returns _true_ if representations _A_ and _B_ can be unified. Otherwise returns _false_. This operator works on level of representations.
For type level variant see [is-same-type](#isSameType).

Example:

~~~
(is-same-type 42 84) ; = true
(is-same-type 42 (construct Int String "84")) ; = false
(is-same-type 42 "84") ; = false
~~~
### <a name="velka.core.langbase.Operators$23"> IsSameType(is-same-type)</a>
Syntax:

~~~
(is-same-type <arg1> <arg2>)
~~~

Type Signature:

~~~
[SYSGENNAMEagu, SYSGENNAMEagv] -> Bool:Native
~~~

Returns _true_ if types _A_ and _B_ can be unified. Otherwise returns _false_. This operator works on level of types, not taking type representations into account.
For representation level variant see [is-same-representation](#isSameRepresentation).

Example:

~~~
(is-same-type 42 84) ; = true
(is-same-type 42 (construct Int String "84")) ; = true
(is-same-type 42 "84") ; = false
~~~
### <a name="velka.core.langbase.Operators$24"> LesserThan(<)</a>
Syntax:

~~~
(< <arg1> <arg2>)
~~~

Type Signature:

~~~
[Int:Native, Int:Native] -> Bool:Native
~~~

Returns _true_ if frist argument is lesser than second argument. Returns _false_ otherwise.

Example:

~~~
(< 42 1) ; = #f
~~~
### <a name="velka.core.langbase.Operators$25"> Log(log)</a>
Syntax:

~~~
(log <message>)
~~~

Type Signature:

~~~
[String:Native] -> []
~~~

Logs a message. [init-logger](#initLogger) must be called before use of _log_.

Example:

~~~
>(init-logger "test-log.log")
[]
>(log "test message")
[]
~~~
### <a name="velka.core.langbase.Operators$26"> Modulo(mod)</a>
Syntax:

~~~
(mod <arg1> <arg2>)
~~~

Type Signature:

~~~
[Int:Native, Int:Native] -> Int:Native
~~~

Returns a remainder after an integer division.

Example:

~~~
(mod 43 3) ; = 1
~~~
### <a name="velka.core.langbase.Operators$27"> Multiplication(*)</a>
Syntax:

~~~
(* <arg1> <arg2>)
~~~

Type Signature:

~~~
[Int:Native, Int:Native] -> Int:Native
~~~

Multiplies two integers.

Example:

~~~
(* 6 7) ; = 42
~~~
### <a name="velka.core.langbase.Operators$28"> Not(not)</a>
Syntax:

~~~
(not <arg>)
~~~

Type Signature:

~~~
[Bool:Native] -> Bool:Native
~~~

Logical not of argument.

Example:

~~~
(not #t) ; = #f
(not (equals? 42 "42")) ; = #t
~~~
### <a name="velka.core.langbase.Operators$29"> NumericEqual(=)</a>
Syntax:

~~~
(= <arg1> <arg2>)
~~~

Type Signature:

~~~
[Int:Native, Int:Native] -> Bool:Native
~~~

Compares two integers for equality.

Example:

~~~
(= 42 42) ; = #t
~~~
### <a name="velka.core.langbase.Operators$30"> ParseInt(parse-int)</a>
Syntax:

~~~
(parse-int <string>)
~~~

Type Signature:

~~~
[String:Native] -> Int:Native
~~~

Tries to parse string into integer. Throws error if string cannot be parsed.

Example:

~~~
>(parse-int "42") ;; = 42
~~~
### <a name="velka.core.langbase.Operators$31"> PrintlnOperator(println)</a>
Syntax:

~~~
(println <arg>)
~~~

Type Signature:

~~~
[SYSGENNAMEc] -> Int:Native
~~~

Prints its argument to standard output with endline and returns number of printed characters.

Example:

~~~
(println "foo") ; prints "foo" and returns 5
~~~
### <a name="velka.core.langbase.Operators$32"> ReadFile(read-file)</a>
Syntax:

~~~
(read-file <filename>)
~~~

Type Signature:

~~~
[String:Native] -> String:Native
~~~

Reads contents of file specified by _filename_ and returns it as string.

Example:

~~~
>(read-file "foo.txt")
"foo bar baz"
~~~
### <a name="velka.core.langbase.Operators$33"> StrSplit(str-split)</a>
Syntax:

~~~
(str-split <string> <by>)
~~~

Type Signature:

~~~
[String:Native, String:Native] -> List:Native
~~~

Splits _string_ by _by_ into a List:Native.

Example:

~~~
>(str-split "foo;bar;baz" ";")
["foo" ["bar" ["baz" []]]]
~~~
### <a name="velka.core.langbase.Operators$34"> Subtraction(-)</a>
Syntax:

~~~
(- <arg1> <arg2>)
~~~

Type Signature:

~~~
[Int:Native, Int:Native] -> Int:Native
~~~

Subtracts _arg2_ from _arg1_.

Example:

~~~
(- 43 1) ; = 42
~~~
### <a name="velka.core.langbase.Operators$35"> Timestamp(timestamp)</a>
Syntax:

~~~
(timestamp)
~~~

Type Signature:

~~~
[] -> Int:Native
~~~

Returns current System/currentTimeMillis wrapped from java.

Example:

~~~
(timestamp) ; = 1658062149471
~~~
### <a name="velka.core.langbase.Operators$36"> ToStr(to-str)</a>
Syntax:

~~~
(to-str <expr>)
~~~

Type Signature:

~~~
[SYSGENNAMEasw] -> String:Native
~~~

Returns readable representation of its argument.

Example:

~~~
>(to-str 42)
"42"
>(to-str (construct List Native))
"[]"
~~~
### <a name="velka.core.langbase.Operators$37"> UnsignedBitShiftRight(ushr)</a>
Syntax:

~~~
(ushr <bits> <n>)
~~~

Type Signature:

~~~
[Int:Native, Int:Native] -> Int:Native
~~~

 Bitwise shift right, without sign-extension. 

Example:

~~~
(ushr -1 3) ;; = 2305843009213693951
~~~
### <a name="velka.core.langbase.Operators$38"> typeStr(type-str)</a>
Syntax:

~~~
(type-str <arg>)
~~~

Type Signature:

~~~
[SYSGENNAMEaut] -> String:Native
~~~

Returns string with type of its argument. This is NOT a special form and the argument will be evaluated.

Example:

~~~
(type-str 1) ;; = "Int:*"
~~~
### <a name="velka.core.langbase.Operators$39"> representationStr(representation-str)</a>
Syntax:

~~~
(representation-str <arg>)
~~~

Type Signature:

~~~
[SYSGENNAMEavs] -> String:Native
~~~

Returns string with representation of its argument. This is NOT a special form and the argument will be evaluated.

Example:

~~~
(representation-str 1) ;; = "Int:Native"
~~~
### <a name="velka.core.langbase.Operators$40"> substr(substr)</a>
Syntax:

~~~
(substr <str> <start-index> <end-index>)
~~~

Type Signature:

~~~
[String:Native, Int:Native, Int:Native] -> String:Native
~~~

Returns a string that is a substring of this string.

Example:

~~~
(substr "hamburger" 4 8) ;; = "urge"
~~~
### <a name="velka.core.langbase.Operators$41"> strlen(strlen)</a>
Syntax:

~~~
(strlen <str>)
~~~

Type Signature:

~~~
[String:Native] -> Int:Native
~~~

Returns the length of this string. The length is equal to the number of Unicode code units in the string.

Example:

~~~
(strlen "hamburger") ;; = 9
~~~