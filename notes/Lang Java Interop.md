# Lang Java Interop

Since creating all the libraries for all various task is beyond scope of my language I decided to introduce way for inteoperability of my langugage and Java platform on which my language is based.

As always there are two parts of the language (interpretation and compilation into Clojure) and several issues to solve. These include:

* How will foregin code be called (during interpretation and compilation)
* How should I convert Java values into lang values and vice versa?
* What syntax will we use for calling foregin code (syntax)
* How will the foregin types be handled?
* What foregin symbols and types should be loaded?
* How should they be loaded? (syntax)
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
