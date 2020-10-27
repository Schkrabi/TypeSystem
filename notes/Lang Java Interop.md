# Lang Java Interop

# Interop was canceled, all notes here are deprecated and not implemented

Since creating all the libraries for all various task is beyond scope of my language I decided to introduce way for inteoperability of my langugage and Java platform on which my language is based.

As always there are two parts of the language (interpretation and compilation into Clojure) and several issues to solve. These include:

* How will foregin code be called (during interpretation and compilation)
* How should I convert Java values into lang values and vice versa?
* What syntax will we use for calling foregin code (syntax)
* How will the foregin types be handled?
* What foregin symbols and types should be loaded?
* Where should be foregin symbols loaded? (environments?)

## How to call foregin code

This issue differs greatly for interpretation and compilation:

### Call foregin code in interpretation

For interpretation Java reflection will be used. There are several basic kinds of values that can be bound to symbol from Java classes:

* classes 
  * will be handled as types, see later
* class variables
  * will be handled in types as their values
* constants
  * will be handled in types as their values 
* static class variables and constants
  * will be bound to specific symbol in same environment as class methods are bound
  * values are converted to lang values
* methods
  * will be bound to specific symbol in the import environment (see where should be foregin symbols loaded?)  -> this might not be sufficient, lang is lacking overloading functions as functions are simply lambdas bound to symbols, Clojure is using special form . (dot) to access class members. Will probably adopt the same approcach.
  * new Abstraction subclass will be created holding class reference and method object to apply the method
  * first argument will always be of the class type
* static methods
  * same as methods but without class type as first argument

### Call foregin code in compilation

I use standard clojure java interop syntax.see on https://clojure.org/reference/java_interop.

* classes
  * for construction we will use 'new' clojure special form

## Converting Java values to Lang values and vice versa

Primitive values will be mapped directly to primitive language types, as follows:

| Java Type | Lang Type             |
|-----------|-----------------------|
| Boolean   | Bool:Native           |
| Byte      | Int:Native            |
| Character | Int:Native            |
| Double    | Double:Native         |
| Float     | Double:Native         |
| Integer   | Int:Native            |
| Long      | Int:Native            |
| Short     | Int:Native            |
| String    | String:Native         |
| Void      | [] (Empty Type Tuple) |

Class instances (objects) will be mapped to speical literals (LitInteropObject), which will hae type of class of this object and Native representation.

This should also cover enumerations.

## Syntax for calling foregin code

There are several things that we should be able to do from lang when accessing interop code:

* load Java symbols
* create instance of Java class
* call class method
* call static method
* access class member (variable, const or method)
* access static class member (variable, const or method)

### load Java Symbols

Java symbols will be loaded by class. For simplicity I would like to have something like '(import lib.name)' or '(require lib.name)'. But since lang facilities for this is much more primitive than Clojure framework I would rather suggest former.

### create instance of Java class

This should be achieved by basic construct call, since Java classes will be handled as types in the lang.

~~~
(construct Java.util.LinkedList Native) => linked list interop value 
~~~

### Call class method
Introducing a new special form 'interop-member-call' with following syntax:

~~~
  (interop-member-call <method-name> <class-instance> <args...>)
  (interop-member-call )
~~~
 
