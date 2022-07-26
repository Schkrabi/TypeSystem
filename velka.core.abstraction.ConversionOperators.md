# General Conversions
Conversions of build-in representations

## Table of Contents
* [IntNativeToIntRoman(IntNative2IntRoman)](#velka.core.abstraction.ConversionOperators$1)
* [IntNativeToIntString(IntNative2IntString)](#velka.core.abstraction.ConversionOperators$2)
* [IntRomanToIntNative(IntRoman2IntNative)](#velka.core.abstraction.ConversionOperators$3)
* [IntRomanToIntString(IntRoman2IntString)](#velka.core.abstraction.ConversionOperators$4)
* [IntStringToIntNative(IntString2IntNative)](#velka.core.abstraction.ConversionOperators$5)
* [IntStringToIntRoman(IntString2IntRoman)](#velka.core.abstraction.ConversionOperators$6)



## Operators
### <a name="velka.core.abstraction.ConversionOperators$1"> IntNativeToIntRoman(IntNative2IntRoman)</a>
Syntax:

~~~
(IntNative2IntRoman <arg>)
~~~

Type Signature:

~~~
[Int:Native] -> Int:Roman
~~~

Converts integer in native represetation into roman representation. This is shorthand for _(convert Int:Native Int:Roman arg)_.

Example:

~~~
(IntNative2IntRoman 42) ; = "XLII"
~~~
### <a name="velka.core.abstraction.ConversionOperators$2"> IntNativeToIntString(IntNative2IntString)</a>
Syntax:

~~~
(IntNative2IntString <arg>)
~~~

Type Signature:

~~~
[Int:Native] -> Int:String
~~~

Converts integer in native represetation into string representation. This is shorthand for _(convert Int:Native Int:String arg)_.

Example:

~~~
(IntNative2IntString 42) ; = "42"
~~~
### <a name="velka.core.abstraction.ConversionOperators$3"> IntRomanToIntNative(IntRoman2IntNative)</a>
Syntax:

~~~
(IntRoman2IntNative <arg>)
~~~

Type Signature:

~~~
[Int:Roman] -> Int:Native
~~~

Converts integer in roman represetation into native representation. This is shorthand for _(convert Int:Roman Int:Native arg)_.

Example:

~~~
(IntRoman2IntNative (construct Int Roman "XLII")) ; = 42
~~~
### <a name="velka.core.abstraction.ConversionOperators$4"> IntRomanToIntString(IntRoman2IntString)</a>
Syntax:

~~~
(IntRoman2IntString <arg>)
~~~

Type Signature:

~~~
[Int:Roman] -> Int:String
~~~

Converts integer in roman represetation into string representation. This is shorthand for _(convert Int:Roman Int:String arg)_.

Example:

~~~
(IntRoman2IntString (construct Int Roman "XLII")) ; = "42"
~~~
### <a name="velka.core.abstraction.ConversionOperators$5"> IntStringToIntNative(IntString2IntNative)</a>
Syntax:

~~~
(IntString2IntNative <arg>)
~~~

Type Signature:

~~~
[Int:String] -> Int:Native
~~~

Converts integer in string represetation into native representation. This is shorthand for _(convert Int:String Int:Native arg)_.

Example:

~~~
(IntString2IntNative (construct Int String "42")) ; = 42
~~~
### <a name="velka.core.abstraction.ConversionOperators$6"> IntStringToIntRoman(IntString2IntRoman)</a>
Syntax:

~~~
(IntString2IntRoman <arg>)
~~~

Type Signature:

~~~
[Int:String] -> Int:Roman
~~~

Converts integer in string represetation into roman representation. This is shorthand for _(convert Int:String Int:Roman arg)_.

Example:

~~~
(IntString2IntRoman (construct Int String "42")) => "XLII"
~~~