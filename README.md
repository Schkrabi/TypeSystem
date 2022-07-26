# Type System

Version 0.1.0

## Manual
### Compilation
Project is formed from several sub-projects:

* **velka.util**
* **velka.types**
* **velka.core**
* **velka.clojure**
* **velka.parser**
* **velka.compiler**
* **velka.test**

_velka.compiler_ is frontend for the functionality implemented in libraries. _velka.util_ and _velka.types_ are required by generated clojure code. _velka.test_ is suite for unit test of the compiler, however it is not currently supported from ant.

#### Prerequisities
Library _velka.parser_ requires _antlr4_ to compile and run. Used version is:

* **antlr4**
 Used version 4.7.2-complete.
 
All external libraries should be placed into _lib_ folder in repository root where ant script should be able to find them during build.

_velka.test_ has some other dependant libraries (JUnit5), however, since its build and testing via ant is not yet supported I won't list them here. If you want to run testing suite I suggest to use some IDE that should be able to link you all requirements.
    
#### Build script
A build script _build-velka_ is provided in repository root. You might need to give it executable privilege with _chmod_ first. For Linux based systems use _build-velka.sh_ for Windows use _build-velka.ps1_ (requires Power Shell).

Script will build all velka jars and copy them to _lib_ folder in repository root.

### <a name="usage">Usage</a>

Program can be used in following modes:

* **compile** compiles file into clojure code (compilation only, for runnable clojure code use build)
* **prepare** prepares current folder for clojure project
* **build** prepares current folder for clojure project and compiles code to clojure
* **interpret** interprets file
* **repl** runs repl
* **help** prints usage information

#### REPL
To use interactive interpretr use _repl_ option

    >java -jar velka.lang.compiler.jar repl
    
In repl mode you can evaluate expressions and modify top level environment and type environment (see [define](#define), [type](#type), [representation](#representation), [constructor](#constructor), [conversion](#conversion)).

#### Compiler
Currently only single code file is supported for compilation in velka.

Compilation to clojure consists of three modes *compile*, *prepare* and *build*. First one only compiles given velka code into equvalent clojure code in user.clj file in current directory.

*prepare* option prepares current directory as clojure project for running/compiling compiled velka code. Please note that velka binaries (*velka.util.jar*, *velka.types.jar* and *velka.core.jar*) are assumed to be in current folder for running compiled code via clojure or compilation to java bytecode. If they are located in different location, you will have to modify generated *deps.edn* file replacing lines:

~~~
"./velka.util.jar"
"./velka.types.jar"
"./velka.core.jar"
~~~

with appropriate path.

*build* is most convenient way of compiling velka code into clojure, it combines functionality of *prepare* and *compile* and will move all .clj files correctly with regard to namespaces. For example, see section [first program](#firstProgram).

## <a name="firstProgram">First program</a>
To create a first velka program you will have to have all velka binaries compiled and ready. If you do not have builded velka yet see section [manual](#Manual). Also antlr library must be on a modulepath (simplest way is just to put it into folder alongside velka binaries).

For simplicity move all the binaries to empty folder, this will be the folder where you velka project will be housed. Create a new text file there and name it *program.vlk*. Open the file and put following code there:

~~~
(define main 
    (lambda () 
        (println "Hello World!")))
~~~

This is a classical introductory program, that will print *Hello World!* to output and ends.

### Interpretation
Simplest and fastest way to test velka code is to interpret it. To do so, invoke compiler by following command:

~~~
>java -jar velka.compiler.jar interpret program.vlk
"Hello World!"
~~~

Note however, that interpreter is not very efficient and program will definitelly be slower than compiled clojure code or compiled java bytecode.

### Clojure and JIT compilation
To generate runnable clojure code from your velka source use compiler *build* command in following way.

~~~
>java -jar velka.lang.compiler.jar build program.vlk
~~~

This will create directories *velka* and *classes* in your current folder as well as *deps.edn* file. These are files required for correctly running velka-generated clojure source. You can see your source compiled to clojure in *velka/clojure/user.clj* file.

To run the code (just-in-time clojure compilation) using following command:

~~~
>clj -m velka.clojure.user
"Hello World!"
~~~

This will run clojure, compile your code and runs it one time on JVM.

Names of the code files and namespaces are fixed, velka currently does not support custom namespaces.

### Ahead of time compilation
If you have compiled clojure code, you can generate java bytecode from it, to avoid furher just-in-time compilation later, or to generate jar file. To do so, first run *clj* without arguments.

~~~
>clj
Clojure 1.10.1
user=>
~~~

Now you are in clojure REPL. To compile your velka project use following expression:

~~~
user=>(compile 'velka.clojure.user)
velka.clojure.user
~~~

This will generate *.class* files in classes directory. You can press Ctrl+D to exit clojure repl. To run java bytecode use following command:

~~~
>java -cp `clj -Spath` velka.clojure.user
"Hello World!"
~~~

The *clj -Spath* clause is using clojure to generate classpath for the binaries. You want to take it into account if you are generating .jar file.

Leinigen is currently not supported.

## Language Velka
Language is similar to Scheme, (so far) without macro support and with limited set of build-in functions and operators. 

Weight of the language lies in its type system, separated type and representation definitions and usage of extended-lambda.

For reference of built-in language members, see individual documents.

### <a name="typeSignature">Type and Representation signatures</a>
There are three kind of representation signatures present in language: atomic representations, composite representations and representation variables.

Type signatures are same as representation signatures, either completely ommiting the representation information or using asterix (_*_) as a representation signature for atomic representations.

#### Atomic Representations
Atomic representations in language are described in two ways:

~~~
<TypeName>
<TypeName>:<RepresentationName>
~~~
    
Where former style describes value of _<TypeName>_ type in any representation and is equal to _<TypeName>:*_ in latter signature style.

For example signature of integer represented by string of roman number would be:

~~~
Int:Roman
~~~
    
For example signature of general double type (without specific representation) would be:

~~~
Double
~~~

or

~~~
Double:*
~~~

#### Composite representations
There are two kind of composite representations in Velka: representation tuples and arrow representations (representations of functions).

For representation tuples we use list syntax:

~~~
(<sub-representation>*)
~~~
    
For example 2-tuple containing native integer and native string would be:

~~~
(Int:Native String:Native)
~~~
    
Arrow representations are representations of functions, they use following syntax:

~~~
<formal argument representation> #> <return representation>
~~~
    
Also note that representation of formal argument is ALWAYS a representation tuple, even for functions only accepting one argument.
    
For example function that accept tvo native integers and returns native string has following signature:

~~~
(Int:Native Int:Native) #> String:Native
~~~
    
Composite representations can be arbitrarily nested.

#### Representation variables
In order to allow generic representations (like generic list), representation variables can be intorduced into representation signatures.

Representation variable is a representation that can be unified with arbitrary type, but when it is unified it stays assigned to that representation.

For example, lets have following identity function:

~~~
(lambda (x) x)
~~~

This function has representation of `(A) #> A` where `A` is a representation variable. This allows funtion to accept argument of any representation and return value of the same representation. Howerver aaplication of this function forces unification of this representation variable, so for example:

~~~
((lambda (x) x) 42)
~~~

Is of representation `Int:Native` and during the inference representation variable `A` is unified with `Int:Native` substituing representation of the function to `(Int:Native) #> Int:Native`.

If we wanted to specify name for the representation variable explicitely we can use:

~~~
(let-type (A) (lambda ((A x)) x))
~~~

Where `let-type` is special form that declares and specifies scope of this representation variable. For details on syntax see [let-type](#let-type). Undeclared representation variables are not allowed.

### Lambda expression with representation signatures
Lambda expression in language supports representation signatures on formal arguments of lambda expression. For example lambda that accepts only String arguments:

~~~
(lambda ((String x)) x)
~~~
    
You can also use representation signature with representation specified. For example lambda that only accepts Native representation of String:

~~~
(lambda ((String:Native x)) x)
~~~
    
For lambda expressions representation signatures are strictly not-mandatory.

### Build-in Representations
Following types and their representations are build-in to compiler:

| Type   | Representation | Note                                            |
|--------|----------------|-------------------------------------------------|
| Int    | Native         |                                                 |
| Int    | String         | Integer represented by decimal string           |
| Int    | Roman          | Integer represented by string with roman number |
| String | Native         |                                                 |
| Double | Native         |                                                 |
| Bool   | Native         |                                                 |
| List   | Native         | Native lists used in cost functions.            |
| List   | JavaArray      | Wrapped java.util.ArrayList.                    |
| List   | JavaLinked     | Wrapped java.util.LinkedList.                   |
    
### Types, Representations and Constructors
To define a new type use _type_ special form. For example to define a type called Name:

~~~
(type Name)
~~~
    
Type cannot be instantiated without representation and constructor. To define representation use _representation_ special form. Each type can have multiple representations. For expample to create two representations of type Name called Structured and Unstructured.

~~~
(representation Structured Name)
(representation Unstructured Name)
~~~
    
Representation cannot be instantiated without constructor. Any representation can have multiple constructors distinguished by number of arguments. This is due to requirement for each representation to be able to represent all values any value of given type. To define constructor use _constructor_ special form. For example to define constructors for Name:Structured representation and Name:Unstructured representation.

~~~
(constructor Name Structured 
	((String:Native firstName) (String:Native secondName)) 
		(cons firstName secondName))
(constructor Name Unstructured ((String:Native name)) name)
~~~
    
### Instantiating types, conversions and extended-lambda
To instantiate representation of given type use _construct_ special form. For example to instantiate representations from previous section.

~~~
(construct Name Structured "John" "Doe") => [John Doe]
(construct Name Unstructured "John Doe") => John Doe
~~~
    
Language supports and uses conversions between individual representations. To define conversion between representations use _conversion_ special form. For example to define conversion from Name:Structured to Name:Unstructured representation:

~~~
(conversion Name:Structured Name:Unstructured 
	((Name:Structured x) 
		(construct Name Unstructured (concat 
			(car (deconstruct x (String:Native String:Native))) 
			(cdr (deconstruct x (String:Native String:Native)))))))
~~~
    
To explicitely invoke conversion use _convert_ special form. For example:

~~~
(convert Name:Structured Name:Unstructured 
	(construct Name Structured "John" "Doe")) => JohnDoe
~~~
    
However conversions are mostly invoked implicitely during evaluation of functiona application. For example lets have lambda that only accepts Name:Unstructured argument:

~~~
((lambda ((Name:Unstructured x)) 
		(concat (deconstruct x) "0")) 
	(construct Name Structured "John" "Doe")) => JohnDoe0
~~~
    
System tries to convert any arguments to required representations and then applies function to converted arguments. If conversion does not exists error is thrown.

Extended-lambda is special form, allowing us to create functions which can behave specifically to representations of its arguments. For example:

~~~
(define f (extended-lambda ((Name x)) 
							((Name:Structured) "Structured")
							((Name:Unstructured) "Unstructured")))
							
(f (construct Name Structured "John" "Doe")) => Structured
(f (construct Name Unstructured "John Doe")) => Unstructured
~~~

### Equality
When using _equals?_ operator, equality is checked in following way:

#### Literals
For primitive literals (Native Integers, Native Doubles, Native Strings and Native Booleans), equality is checked by value. Internally Java _equals_ method (for interpretation) or Clojure _=_ operator (for compilation) is used.  

Example:

~~~
(equals? 42 42) ; = #t
(equals? 3.14 2.24) ; = #f
(equals? #t #t) ; = #t
(equals? "foo" "bar") ; = #f
~~~

For composite literals first, type of both values are checked. If they equals, then underlying value is checked for equality.

Example:

~~~
(equals? 
    (construct Name Structured "Jane" "Doe") 
    (construct Name Structured "Jane" "Doe")) ; = #t
(equals? 
    (construct Name Unstructured "Jane Doe") 
    (construct Name Unstructured "Muhamad Lee")) ; = #f
(equals? (construct Name Unstructured "42") (construct Int String "42")) ; = #f
~~~

#### Tuples
Tuples are equal if they have the same length and each their value equals.

Example:

~~~
(equals? (cons 42 "foo") (cons 42 "foo")) ; = #t
(equals? (cons 42 "foo") (cons "foo" 42)) ; = #f
~~~

#### Functions and extended functions
Functions are equal if its following parts are equal:

* argument list
* arguemnt types
* body
* creation environment

Also please note that automatically generated names of type variables are generated to be unique, so anonymous functions without specified argument list might be counter intuitivelly not equal. Also declared type variables in _let-type_ are substitued to be unique, so type variables declared in different let types are different, even though they have same name.

Example:

~~~
(def f (lambda (x) x))
(equals? x x) ; = #t
(equals? 
    (lambda (x) x) 
    (lambda (x) x)) ; = #f generated argument list type variables are not equal
    
(equals? 
    (let-type (A) (lambda ((A x)) x)) 
    (let-type (A) (lambda ((A x)) x))) ; = #f type variables declared in 
                                       ;      different let-types are considered 
                                       ;      different

(let-type (A) (equals? (lambda ((A x)) x) (lambda ((A x)) x))) ; = #t
(equals? (lambda (x) y) (lambda (x) x)) ; = #f body does not equals
(equals? (lambda (y) x) (lambda (x) x)) ; = #f argument list does not equals

(equals? 
    ((lambda () (lambda (x) x))) 
    (lambda (x) x)) ; = #f creation environment does not equals
    
(equals? 
    (lambda ((Int:Native x)) x) 
    (lambda ((String:Native x)))) ; = #f argument list does not equals
~~~

### Extended function application
Extended function can have multiple different implementations depending on representations of arguments it is applied to. They are created by special form [_extended-lambda_](#extendedLambda). For example:

~~~
(def foo (extended-lambda (Int)))
~~~

Actual implementations for extended functions are defined by [_extend_](#extend) special form, where implementation is defined by lambda expression (or any expression yielding a function)

~~~
(def foo (extend foo
			(lambda ((Int:Native x)) "This is Int:Native")))
(def foo (extend foo 
			(lambda ((Int:String x)) "This is Int:String")))
    
(foo 42) ; = "This is Int:Native"
(foo (construct Int String "42")) ; = "This is Int:String"
~~~

Extended functions are usefull when dealing with data that can come in various different implementation. Extended functions allow us to use specific algorithms for specific representations of data. For example binary matrix can be represented by two dimensional array, bit array or incidence list. Extended functions allow us to tranparently implement functions using efficient algorithms based on representations of arguments.

#### Implementation selection
When extended function is applied, it is using auxiliary *cost functions* to rank and select implementation that will be used. Each implementation has its own cost function. Cost function maps arguments of application to integer. Then implementation with least rank is selected and applied.

If no cost function is specified, default cost function is used for implementation. Default cost function compares representations of supplied arguments and formal arguments of implementations and increases cost by _1_ for each representation that is different. Therefore if supplied arguments have exactly same representation as formal arguments of implementations default cost function will return _0_. On the other hand if representation of each supplied argument and formal argument of implementation is different, default cost function will return number equal to number of arguments.

#### <a name="rankingFunction">Cost function</a>
Lets have an extended function with type signature _A #> B_. Then cost function for any implementation of this extended function is any function or extended function with representation signature:ture:

~~~
A #> Int:Native
~~~

During each application of extended function cost function is called with application argument for each implementation of that extended function. Then implementation which cost-function yielded lowest result is called with the argument of application.

~~~
((extend (extend (extended-lambda (Int)) 
			(lambda ((Int:Native x)) "Native"))
			(lambda ((Int:Roman x)) "Roman"))
    (construct Int Roman "XLII"))
~~~

First argument _(construct Int Roman "XLII")_ will evaluate, yielding value _"XLII"_ of representation _Int:Roman_. Then this value is passed as argument to cost function of each implementation. Because this extended function is using default cost functions first const function will yield _1_ for _(lambda ((Int:Native x)) "Native")_ and _0_ for _(lambda ((Int:Roman x)) "Roman")_. Therefore extended function will select second implementation and whole expression will yield _"Roman"_

#### Specifing cost function
Cost function can be specified as second argument of the _extend_ special form. For cost function it is usually best practice not to use representations (but only types) on their arguments. This is because cost function are applied as normal functions during evaluation and therefore unvanted automatic conversions might occur.

For example:

~~~
((extend (extend (extended-lambda (Int))
			(lambda ((Int:Native x)) "Native") (lambda ((Int:* x)) (if (instance-of-representation x Int:Native) 0 1)))
			(lambda ((Int:Roman x)) "Roman") (lambda ((Int:* x)) 42))
	(construct Int Roman "XLII"))
~~~

In this case _(lambda ((Int:* x)) (if (instance-of-representation x Int:Native) 0 999))_ is applied with _"XLII"_ of _Int:Roman_ representation, yielding _1_ (somewhat mimicking default cost function. Also _(lambda ((Int:* x)) 42)_ is applied with _"XLII"_ yielding _42_. Therefore first implementation will be selected and whole application will yield _"Native"_.

To demonstrate caveats of using representations in cost function arguments, consider following code.

~~~
((extend (extend (extended-lambda (Int))
			(lambda ((Int:Native x)) "Native") (lambda ((Int:Native x)) (if (instance-of-representation x Int:Native) 0 99)))
			(lambda ((Int:Roman x)) "Roman") (lambda ((Int:Roman x)) 42))
	(construct Int Roman "XLII"))
~~~

In this case _(lambda ((Int:Native x)) (if (instance-of-representation x Int:Native) 0 99))_ will always yield _0_ because when applying this cost function, representation of _x_ is always converted to _Int:Native_, therefore this application will yield "Native", even though we might intuitively expect it to yield _"Roman"_.
    
[antlr4]: https://www.antlr.org/download.html
