# Type System

Version 0.1.0

## Manual
### Compilation
Project is formed from three sub-projects:

* **velka.lang.util**
* **velka.lang.types**
* **velka.lang.compiler**

_velka.lang.util_ and _velka.lang.types_ are libraries used for _velka.lang.compiler_ as well as any clojure code generated with compiler. _velka.lang.compiler_ is an executable jar, for more details see [usage](#usage).

#### Prerequisities
Library _velka.lang.util_ does not have any prerequisities (other that core java). _velka.lang.types_ requires _velka.lang.util_. 

As for _velka.lang.compiler_ it requires two beforementioned velka libraries as well as following external libraries:

* **antlr4**
 Used version 4.7.2-complete.
* **apiguardian.api** 
 Used version 1.7.0. 
* **junit.jupiter.api**
 Used version 5.7.0. 
* **junit.platform.commons**
 Used version 1.7.0. 
* **opentest4j**
 Used version 1.2.0. 
* **junit.platform.console**
 Used version 1.7.0.
 
All external libraries should be placed into _lib_ folder in repository root where ant script should be able to find them during build.
    
#### Build script
A build script _build-velka.sh_ is provided in repository root. You might need to give it executable privilege with _chmod_ first. Build on not-bash platforms is currently not supported.

Script will build all three velka jars and copy them to _lib_ folder in repository root.

### <a name="usage">Usage</a>

Program can be used in following modes:

* **compile** compiles file into clojure code (compilation only, for runnable clojure code use build)
* **prepare** prepares current folder for clojure project
* **build** prepares current folder for clojure project and compiles code to clojure
* **interpret** interprets file
* **repl** runs repl
* **help** prints usage information

#### REPL
To use interpreter simply start program without arguments:

    >java -jar velka.lang.compiler.jar
    
In interpreter mode you can evaluate expressions and modify top level environment and type environment (see [define](#define), [type](#type), [representation](#representation), [constructor](#constructor), [conversion](#conversion)).

#### Compiler
Currently only single code file is supported for compilation in velka.

Compilation to clojure consists of three modes *compile*, *prepare* and *build*. First one only compiles given velka code into equvalent clojure code in user.clj file in current directory.

*prepare* option prepares current directory as clojure project for running/compiling compiled velka code. Please note that velka binaries (*velka.lang.util.jar* and *velka.lang.types.jar*) are assumed to be in current folder for running compiled code via clojure or compilation to java bytecode. If they are located in different location, you will have to modify generated *deps.edn* file replacing lines:

~~~
"./velka.lang.util.jar"
"./velka.lang.types.jar"
~~~

with appropriate path.

*build* is most convenient way of compiling velka code into clojure, it combines functionality of *prepare* and *compile* and will move all .clj files correctly with regard to namespaces. For example, see section [first program](#firstProgram).

## <a name="firstProgram">First program</a>
To create a first velka program you will have to have all three velka binaries: *velka.lang.util.jar*, *velka.lang.types.jar* and *velka.lang.compiler.jar* compiled and ready. If you do not have builded velka yet see section [manual](#Manual). Also antlr library must be on a classpath (simplest way is just to put it into folder alongside velka binaries).

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
>java -jar velka.lang.compiler.jar interpret program.vlk
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

### Special forms
Here is reference to all special form present in language.

* [and](#and)
* [can-deconstruct-as](#canDeconstructAs)
* [cons](#cons)
* [construct](#construct)
* [convert](#convert)
* [constructor](#constructorSpecForm)
* [conversion](#conversion)
* [deconstruct](#deconstruct)
* [define](#define)
* [eapply](#eapply)
* [error](#error)
* [extended-lambda](#extendedLambda)
* [extended-lambda-ranking](#extendedLambdaRanking)
* [get](#get)
* [if](#if)
* [instance-of](#instanceOf)
* [instance-of-representation](#instanceOfRepresentation)
* [lambda](#lambda)
* [let](#let)
* [let*](#let-ast)
* [let-type](#letType)
* [or](#or)
* [representation](#representation)
* [tuple](#tuple)
* [type](#type)

#### <a name="and">and</a>
Syntax:

~~~
(and <arg1> <arg2>)
~~~
Type signature:

~~~
(Bool:Native Bool:Native) #> Bool:Native
~~~

Represents logical and of two values. If first argument evaluates to _false_ second argument is not evaluated.

Example:

~~~
(and #t #f)
~~~

#### <a name="canDeconstructAs">can-deconstruct-as</a>
Syntax:

~~~
(can-deconstruct-as <expression> <type-signature>)
~~~
Where:

* _expression_ expresion which is evaluated and its value can be deconstructed as a specified type
* _type-signature_ [type signature](#typeSignature)

Checks if value in the expression can be deconstructed as specified type. Returns _Bool:Native_.

Example:

~~~
(can-deconstruct-as 
    (construct Name Structured "Jane" "Doe") 
    (String:Native String:Native))
~~~

#### <a name="cons">cons</a>

~~~
(cons <arg1> <arg2>)
~~~
Type signature:

~~~
(A B) #> (A B)
~~~

Creates pair (a 2-tuple) of its arguments.

Example:

~~~
(cons 42 "foo")
~~~

#### <a name="construct">construct</a>
Syntax:

~~~
(construct <type> <representation> <arg1>...)
~~~

Constructs value of given type and representation based on previously defined constructor. Also see [constructor](#constructorSpecForm)

Example:

~~~
(construct Int Native 42)
(construct Name Structured "Jane" "Doe")
~~~

#### <a name="convert">convert</a>
Syntax:

~~~
(convert <from-representation> <to-representation> <value>)
~~~

Converts its argument into another type representation if such conversion was previously defined. Also see [conversion](#conversion)

Example:

~~~
(convert Int:Native Int:Roman 42)
~~~

#### <a name="constructorSpecForm">constructor</a>
Syntax:

~~~
(constructor 
    <type> 
    <representation> 
    (<constructor arguments>...) 
    <constructor body>)
~~~

Defines constructor for given type and representation

Example:

~~~
(constructor 
    Name 
    Structured 
    ((String:Native first) (String:Native second)) 
    (cons first second))
~~~

#### <a name="conversion">conversion</a>
Syntax:

~~~
(conversion 
    <from-representation> 
    <to-representation> 
    (<conversion argument>) 
    <conversion body>)
~~~

Defines conversion from one representation to another.

Example:

~~~
(conversion Name:Structured Name:Unstructured ((Name:Structured x)) 
	(construct Name Unstructured 
		(concat 
			(car (deconstruct x (String:Native String:Native))) 
			(cdr (deconstruct x (String:Native String:Native))))))
~~~

#### <a name="deconstruct">deconstruct</a>
Syntax:

~~~
(deconstruct <expression> <type-signature>)
~~~
Where:

* _expression_ is expression which will be evaluated and deconstructed to given type. 
* _type-signature_ is [type signature](#typeSignature)

This special form attemps to deconstruct given value to a given type. Due to compiler limitations there is no check at compile time if the deconstruction is correct and user should use special form [_can-deconstruct-as_](#canDeconstructAs) to check if deconstruction is valid at runtime.

Example:

~~~
(deconstruct (construct Int Roman "XLII") String:Native)
~~~

#### <a name="define">define</a>
Sytax:

~~~
(define <symbol> <expression>)
~~~
Where:

* _symbol_ is symbol to which binding is created in top level environment
* _expression_ is expression which will be evaluated and  value will be bound to symbol

Binds value to symbol in top-level environment.

Example:

~~~
(def x 10)
(def identity (lambda (x) x))
~~~

#### <a name="eapply">eapply</a>
Syntax:

~~~
(eapply <abstraction> <arguments>)
~~~

OR

~~~
(eapply <abstraction> <arguments> <ranking-function>)
~~~

Where:

* _abstraction_ is an expression that evaluates into function or extended-function
* _arguments_ is an exprpression that evaluates into tuple of arguments
* _ranking-function_ is an expression that evaluates into [ranking function](#rankingFunction)

This is application special form, it has two main purposes:

* application of function or extended function with arguments in a tuple
* usage of specific ranking function for application

Example:

~~~
(eapply (lambda (x y) x) (cons 42 "foo"))
(eapply 
    (extended-lambda ((Int x) (Int y)) 
        ((Int:Native Int:Native) "native")
        ((Int:Roman Int:Roman) "roman))
    (cons 42 42)
    (lambda ((List:Native formal-arg-list) (List:Native real-arg-list))
        (if (instance-of-representation (head-list formal-arg-list) Int:Roman)
            0
            999)))
~~~

#### <a name="error">error</a>
Syntax:

~~~
(error <expression>)
~~~
Where:

* _expression_ is an expression that yields _String_ and is contained in error message.

Throws user defined exception.

Example:

~~~
(define safe-div
	(lamdba (x y)
		(if (= y 0)
			(error "Error, division by zero.")
			(/ x y))))
~~~

#### <a name="extendedLambda">extended-lambda</a>
Syntax:

~~~
(extended-lambda 
    (<argument-list>) 
    ((<argument-representation-list>) <implementation>)...)
~~~
Where:

* _argument-list_ is list of typed arguments without specified representations
* _argument-representation-list_ is list of argument representations used for a specific implementation
* _implementation_ is a body of function for specific representations of arguments

Creates extended function that is either directly applicable or can be bound to symbol via _define_.

Example:

~~~
(extended-lambda ((Int x))
	((Int:Native) (+ x 1))
	((Int:String) (construct 
                    Int 
                    String 
                    (concat "1" (deconstruct x String:Native)))))
~~~

#### <a name="extendedLambdaRanking">extended-lambda-ranking</a>
Syntax:

~~~
(extended-lambda-ranking 
    (<argument-list>) 
    <ranking-function> 
    ((<argument-representation-list>) <implementation>)...)
~~~

Where:

* _<argument-list>_ is list of typed arguments without specified representations
* _<ranking-function>_ is an expression that evaluates into [ranking function](#rankingFunction)
* _<argument-representation list>_ is list of argument representations used for a specific implementation
* _<implementation>_ is a body of function for specific representations of arguments

Creates extended function similar to [_extended-lambda_](#extendedLambda) but it will use _<ranking-function>_ for implementation selection, unless it is overriden by usage of [eapply](#eapply) special form.

Example:

~~~
(extended-lambda-ranking
    ((Int x) (Int y))
    (lambda ((List:Native formal-arg-list) (List:Native real-arg-list))
        (if (instance-of-representation (head-list formal-arg-list) Int:Roman)
            0
            999))
    ((Int:Native Int:Native) "native")
    ((Int:Roman Int:Roman) "roman))
~~~

#### <a name="get">get</a>
Syntax:

~~~
(get <tuple> <index>)
~~~
Where:

* _tuple_ is expression that evaluates to tuple
* _index_ is expression that evaluates to Int:Native

Gets _index_'th value from a tuple. Indexes are zero based, causes error if index is out of bounds.

Example:

~~~
>(get (tuple 42 "a" #t) 1)
"a"
~~~

#### <a name="if">if</a>
Sytax:

~~~
(if <condition> <true-branch> <false-branch>)
~~~
Where:

* _condition_ is an expression that yields _Bool_
* _true-branch_ is an expression that will be evaluated if _<condition>_ yields _true_
* _false-branch_ is an expression that will be evaluated if _<codition>_ yields _false_

Traditional branchinch expression, _condition_ is always evaluated, if it yields _true_ _true-branch_ is evaluated, otherwise _false-branch_ is evaluated.

Example:

~~~
(lambda (x)
	(if (= x 0)
		"zero"
		"non-zero"))
~~~

#### <a name="instanceOf">instance-of</a>
Syntax:

~~~
(instance-of <expression> <type-signature>)
~~~

Where

* _expression_ is an arbitrary expression
* _type-signature_ is a [type signature](#typeSignature)

This special form evaluates to _true_ if _expression_ is of type _type-signature_. Otherwise it evaluates to _false_.

This special form is working only with types, it is not taking representation information into account, meaning expression yielding different representation of the same base type will evaluate to _true_. 

For version granular to representation level see [instance-of-representation](#instanceOfRepresentation).

Example:

~~~
(instance-of 42 Int:Native) ; = true
(instance-of (construct Int String "42") Int:Native) ; = true
(instance-of "42" Int:Native) ; = false
~~~

#### <a name="instanceOfRepresentation">instance-of-representation</a>
Syntax:

~~~
(instance-of <expression> <type-signature>)
~~~

Where

* _expression_ is an arbitrary expression
* _type-signature_ i a [type signature](#typeSignature)

This special form evaluates to _true_ if _expression_ is of type and representation _type-signature_. Otherwise it evaluates to false.

This special form is working with both types and representations. This means expressions yielding different representation of the same base type will evaluate to _false_.

For version only taking types into account see [instance-of](#instanceOf).

~~~
(instance-of-representation 42 Int:Native) ; = true
(instance-of-representation (construct Int String "42") Int:Native) ; = false
(instance-of-representation "42" Int:Native) ; = false
~~~

#### <a name="lambda">lambda</a>
Syntax:

~~~
(lambda (<argument-list>) <body>)
~~~
Where:

* _argument-list_ is a list of either typed or untyped arguments
* _body_ is expression evaluated when the function is applied

Creates a function.

Examples:

~~~
(lambda (x) x)
(lambda ((Int x) (Int y)) (+ x y))
(lambda ((Name:Structured name)) 
    (car (deconstruct name (String:Native String:Native))))
(lambda (value (List l)) (append-list value l))
~~~

#### <a name="let">let</a>
Syntax:

~~~
(let ((<var1> <exp1>)...) <body>)
~~~
Where:

* _var_ is variable name
* _exp_ is an arbitrary expression
* _body_ is and arbitrary expression

Special form for binding values in scope of _body_. Values are bound all at once and cannot reference previously bound values in the same let (see [let*](#let-ast)).

Example:

~~~
>(let
    ((x 10)
     (y 5))
    (+ x y))
15
~~~

#### <a name="let-ast">let*</a>
Syntax:

~~~
(let* ((<var1> <exp1>)...) <body>)
~~~
Where:

* _var_ is variable name
* _exp_ is an arbitrary expression
* _body_ is and arbitrary expression

Special form for binding values in scope of _body_. Values are bound one after another and later bound variables can refer to previously bounded ones. Also see [let](#let).

Example:

~~~
>(let
    ((x 10)
     (y (* x 2)))
    (+ x y))
30
~~~


#### <a name="letType">let-type</a>
Syntax:

~~~
(let-type (<list-of-type-variables>) <body>)
~~~
Where:

* _list-of-type-variables_ is list of declared type variables
* _body_ is expression that is evaluated within scope of type variables

Declares number of type variables and defines its scope. In _body_ you can freely use type variables defined in _let-type_.

Example:

~~~
(let-type (A)
	(lambda ((A x)) x))
~~~

#### <a name="or">or</a>
Syntax:

~~~
(or <arg1> <arg2>)
~~~
Type signature:

~~~
(Bool:Native Bool:Native) #> Bool:Native
~~~

Represents logical or of two values. If first argument evaluates to _true_ second argument is not evaluated.

Example:

~~~
(or #f #t)
~~~

#### <a name="representation">representation</a>
Syntax:

~~~
(representation <representation-name> <type-name>)
~~~
Where:

* _representation-name_ is name of defined representation
* _type-name_ is name of already defined type

Defines new type representation to already defined type. See [type](#type).

Example:

~~~
(representation Structured Name)
~~~

#### <a name="tuple">tuple</a>
Syntax:

~~~
(tuple <arg1>...)
~~~

Creates tuple from given arguments.

Example:

~~~
>(tuple 42 "a" #t)
[42 "a" #t]
~~~

#### <a name="type">type</a>
Syntax:

~~~
(type <type-name>)
~~~

Defines new type.

Examples:

~~~
(type Name)
~~~

### Operators
Operators are build in functions in the language. Following opertaors are available:

* [+ (addition)](#addition)
* [/ (division)](#division)
* [< (lesser than)](#lesserThan)
* [* (multiplication)](#multiplication)
* [= (equals)](#numericalEqual)
* [- (subtraction)](#subtraction)
* [bit-and](#bitAnd)
* [bit-or](#bitOr)
* [bit-not](#bitNot)
* [bit-xor](#bitXor)
* [car](#car)
* [can-unify-representations](#canUnifyRepresentations)
* [can-unify-types](#canUnifyTypes)
* [cdr](#cdr)
* [concat](#concat)
* [equals?](#equals)
* [init-logger](#init-logger)
* [IntNative2IntString](#intnative2intstring)
* [IntNative2IntRoman](#intnative2introman)
* [IntRoman2IntNative](#introman2intnative)
* [IntRoman2IntString](#introman2intstring)
* [IntString2IntNative](#intstring2intnative)
* [IntString2IntRoman](#intstring2introman)
* [is-same-type](#isSameType)
* [is-same-representation](#isSameRepresentation)
* [log](#log)
* [not](#not)
* [parse-int](#parseInt)
* [println](#println)
* [read-file](#readFile)
* [shl](#shl)
* [shr](#shr)
* [str-split](#strSplit)
* [timestamp](#timestamp)
* [to-str](#toStr)

#### <a name="addition">+ (addition)</a>
Syntax:

~~~
(+ <arg1> <arg2>)
~~~
Where:

* _arg1_ and _arg2_ are integers

Type Signature:

~~~
(Int:Native Int:Native) #> Int:Native
~~~

Adds two integers.

Example:

~~~
(+ 21 21) ; = 42
~~~

#### <a name="division">/ (division)</a>
Syntax:

~~~
(/ <arg1> <arg2)
~~~
Where:

* _arg1_ and _arg2_ are integers

Type Signature:

~~~
(Int:Native Int:Native) #> Int:Native
~~~

Divides _arg1_ by _arg2_. if _arg2_ evaluates to zero, causes exception.

Example:

~~~
(/ 84 2) ; = 42
~~~

#### <a name="lesserThan">< (lesser than)</a>
Syntax:

~~~
(< <arg1> <arg2>)
~~~
Where:

* _arg1_ and _arg2_ are integers

Type Signature:

~~~
(Int:Native Int:Native) #> Bool:Native
~~~

Compares two integers.

Example:

~~~
(< 42 1) ; = #f
~~~

#### <a name="multiplication">* (multiplication)</a>
Syntax:

~~~
(* <arg1> <arg2>
~~~
Where:

* _arg1_ and _arg2_ are integers

Type Signature:

~~~
(Int:Native Int:Native) #> Int:Native
~~~

Multiplies two integers.

Example:

~~~
(* 6 7) ; = 42
~~~

#### <a name="numericalEqual">= (equals)</a>
Syntax:

~~~
(= <arg1> <arg2>)
~~~
Where:

* _arg1_ and _arg2_ are integers

Type Signature:

~~~
(Int:Native Int:Native) #> Bool:Native
~~~

Compares two integers for equality.

Example:

~~~
(= 42 42) ; = #t
~~~

#### <a name="subtraction">- (subtraction)</a>
Syntax:

~~~
(- <arg1> <arg2>)
~~~
Where:

* _arg1_ and _arg2_ are integers

Type Signature:

~~~
(Int:Native Int:Native) #> Int:Native
~~~

Subtracts _arg2_ from _arg1_.

Example:

~~~
(- 43 1) ; = 42
~~~

#### <a name="bitAnd">bit-and</a>
Syntax:

~~~
(bit-and <arg1> <arg2>)
~~~
Where

* _arg1_ and _arg2_ are integers

Type Signature:

~~~
(Int:Native Int:Native) #> Int:Native
~~~

Performs bit-wise and of two integers.

Example:

~~~
(bit-and 5 1) ; = 1
~~~

#### <a name="bitOr">bit-or</a>
Syntax:

~~~
(bit-or <arg1> <arg2>)
~~~
Where

* _arg1_ and _arg2_ are integers

Type Signature:

~~~
(Int:Native Int:Native) #> Int:Native
~~~

Performs bit-wise or of two integers.

Example:

~~~
(bit-or 5 1) ; = 5
~~~

#### <a name="car">car</a>
Syntax:

~~~
(car <arg>)
~~~  
Where:

* _arg_ is a pair.

Type Signature:

~~~
((A B)) #> A
~~~

Extracts first value from a pair.

Example:

~~~
(car (cons 42 "42")) ; = 42
~~~

#### <a name="cdr">cdr</a>
Syntax:

~~~
(cdr <arg>)
~~~
Where:

* _arg_ is a pair.

Type Signature:

~~~
((A B)) #> B
~~~

Extracts second value from a pair.

Example:

~~~
(cdr (cons 42 "42")) ; = "42"
~~~

#### <a name="concat">concat</a>
Syntax:

~~~
(concat <arg1> <arg2>)
~~~
Where:

* _arg1_ and _arg2_ are strings

Type Signature:

~~~
(String:Native String:Native) #> String:Native
~~~

Concatenates two strings.

Example:

~~~
(concat "foo" "bar") ; = "foobar"
~~~

#### <a name="equals">equals?</a>
Syntax:

~~~
(equals? <arg1> <arg2>)
~~~

Type Signature:

~~~
(A B) #> Bool:Native
~~~

Compares two values. For more information see [equality](#equality).

Examples:

~~~
(equals? 42 "42") ; = #f
(equals? (cons 42 42) (cons 42 42)) ; = #t
~~~

#### <a name="not">not</not>
Syntax:

~~~
(not <arg>)
~~~
Where:

* _arg_ is a boolean

Type Signature:

~~~
(Bool:Native) #> Bool:Native
~~~

Logical not of argument.

Examples:

~~~
(not #t) ; = #f
(not (equals? 42 "42")) ; = #t
~~~

#### <a name="println">println</a>
Syntax:

~~~
(println <arg>)
~~~
Where:

* _arg_ evaluates to string.

Type Signature:

~~~
(String:Native) #> Int:Native
~~~

Prints its argument to standard output with endline and returns number of printed characters.

Example:

~~~
(println "foo") ; prints "foo" and returns 5
~~~

#### <a name="canUnifyTypes">can-unify-types</a>
Syntax:

~~~
(can-unify-types <arg1> <arg2>)
~~~

Where:

* _<arg1>_ and _<arg2>_ evaluates to [type symbols](#typeSymbol)

Type Signature:

~~~
(A B) #> Bool:Native
~~~

Returns _true_ if types carried by _arg1_ and _arg2_ can be unified. Otherwise returns _false_. This operator works on level of types, not taking type representations into account. Meaning for example _Int:Native_ and _Int:String_ can unify and type symbols carriing them will yield _true_ on application of the operator. 

For representation level variant see [can-unify-representations](#canUnifyRepresentations).

#### <a name="canUnifyRepresentations">can-unify-representations</a>
Syntax:

~~~
(can-unify-representations <arg1> <arg2>)
~~~

Where:

* _arg1_ and _arg2_ are [type symbols](#typeSymbol)

Type Signature:

~~~
(A B) #> Bool:Native
~~~

Returns _true_ if representations carried by _arg1_ and _arg2_ can be unified. Otherwise returns _false_. This operator works on level of representations, meaning for example _Int:Native_ and _Int:String_ cannot unify and type symbols carriing them will yield _false_ on application of the operator.

For type level variant see [can-unify-types](#canUnifyTypes).

#### <a name="isSameType">is-same-type</a>
Syntax:

~~~
(is-same-type <arg1> <arg2>)
~~~

Type Signature:

~~~
(A B) #> Bool:Native
~~~

Returns _true_ if types _A_ and _B_ can be unified. Otherwise returns _false_. This operator works on level of types, not taking type representations into account. 
For representation level variant see [is-same-representation](#isSameRepresentation).

Example:

~~~
(is-same-type 42 84) ; = true
(is-same-type 42 (construct Int String "84")) ; = true
(is-same-type 42 "84") ; = false
~~~

#### <a name="isSameRepresentation">is-same-representation</a>
Syntax:

~~~
(is-same-representation <arg1> <arg2>)
~~~

Type Signature:

~~~
(A B) #> Bool:Native
~~~

Returns _true_ if representations _A_ and _B_ can be unified. Otherwise returns _false_. This operator works on level of representations.
For type level variant see [is-same-type](#isSameType).

Example:

~~~
(is-same-type 42 84) ; = true
(is-same-type 42 (construct Int String "84")) ; = false
(is-same-type 42 "84") ; = false
~~~

#### <a name="IntNative2IntString">IntNative2IntString</a>
Syntax:

~~~
(IntNative2IntString <arg>)
~~~
Where:

* _arg_ evaluates to integer

Type Signature:

~~~
(Int:Native) #> Int:String
~~~

Converts integer in native represetation into string representation. This is shorthand for _(convert Int:Native Int:String arg)_.

Example:

~~~
(IntNative2IntString 42) ; = "42"
~~~

#### <a name="IntNative2IntRoman">IntNative2IntRoman</a>
Syntax:

~~~
(IntNative2IntRoman <arg>)
~~~
Where:

* _arg_ evaluates to integer

Type Signature:

~~~
(Int:Native) #> Int:Roman
~~~

Converts integer in native represetation into roman representation. This is shorthand for _(convert Int:Native Int:Roman arg)_.

Example:

~~~
(IntNative2IntRoman 42) ; = "XLII"
~~~

#### <a name="IntString2IntNative">IntString2IntNative</a>
Syntax:

~~~
(IntString2IntNative <arg>)
~~~
Where:

* _arg_ evaluates to integer

Type Signature:

~~~
(Int:String) #> Int:Native
~~~

Converts integer in string represetation into native representation. This is shorthand for _(convert Int:String Int:Native arg)_.

Example:

~~~
(IntString2IntNative (construct Int String "42")) ; = 42
~~~

#### <a name="IntString2IntRoman">IntString2IntRoman</a>
Syntax:

~~~
(IntString2IntRoman <arg>)
~~~
Where:

* _arg_ evaluates to integer

Type Signature:

~~~
(Int:String) #> Int:Roman
~~~

Converts integer in string represetation into roman representation. This is shorthand for _(convert Int:String Int:Roman arg)_.

Example:

~~~
(IntString2IntRoman (construct Int String "42")) => "XLII"
~~~

#### <a name="IntRoman2IntNative">IntRoman2IntNative</a>
Syntax:

~~~
(IntRoman2IntNative <arg>)
~~~
Where:

* _arg_ evaluates to integer

Type Signature:

~~~
(Int:Roman) #> Int:Native
~~~

Converts integer in roman represetation into native representation. This is shorthand for _(convert Int:Roman Int:Native arg)_.

Example:

~~~
(IntRoman2IntNative (construct Int Roman "XLII")) ; = 42
~~~

#### <a name="IntRoman2IntString">IntRoman2IntString</a>
Syntax:

~~~
(IntRoman2IntString <arg>)
~~~
Where:

* _arg_ evaluates to integer

Type Signature:

~~~
(Int:Roman) #> Int:String
~~~

Converts integer in roman represetation into string representation. This is shorthand for _(convert Int:Roman Int:String arg)_.

Example:

~~~
(IntRoman2IntString (construct Int Roman "XLII")) ; = "42"
~~~

#### <a name="bitNot">bit-not</a>
Syntax:

~~~
(bit-not <arg>)
~~~
Where:

* _arg_ evaluates to integer

Type Signature:

~~~
(Int:Native) #> Int:Native
~~~

Negates all bits in binary representation of the argument.

Example:

~~~
>(bit-not 42)
-43
~~~

#### <a name="bitXor">bit-xor</a>
Syntax:

~~~
(bit-xor <arg1> <arg2>)
~~~
Where:

* _arg1_ evaluates to integer
* _arg2_ evaluates to integer

Type Signature:

~~~
(Int:Native Int:Native) #> Int:Native
~~~

Computes xor of binary representations of integer arguments.

Example:

~~~
>(bit-xor 6 3)
5
~~~

#### <a name="shl">shl</a>
Syntax:

~~~
(shl <bits> <n>)
~~~
Where:

* _bits_ evaluates to integer
* _n_ evaluates to integer

Type Signature:

~~~
(Int:Native Int:Native) #> Int:Native
~~~

Shifts bits of binary representation_bits_ left by _n_ positions.

Example:

~~~
>(shl 1 4)
16
~~~

#### <a name="shr">shr</a>
Syntax:

~~~
(shr <bits> <n>)
~~~
Where:

* _bits_ evaluates to integer
* _n_ evaluates to integer

Type Signature:

~~~
(Int:Native Int:Native) #> Int:Native
~~~

Shifts bits of binary representation_bits_ right by _n_ positions.

Example:

~~~
>(shr 16 4)
1
~~~

#### <a name="timestamp">timestamp</a>
Syntax:

~~~
(timestamp)
~~~

Type Signature:

~~~
() #> Int:Native
~~~

Returns current System/nanoTime wrapped from java.

#### <a name="initLogger">init-logger</a>
Syntax:

~~~
(init-logger <name>)
~~~
Where:

* _name_ evaluates to string

Type Signature:

~~~
(String:Native) #> ()
~~~

Initializes logger, which will write to file specified by _name_. For logging messages see [log](#log).

Example:

~~~
>(init-logger "test-log.log")
[]
~~~

#### <a name="log">log</a>
Syntax:

~~~
(log <message>)
~~~
Where:

* _message_ evaluates to string

Type Signature:

~~~
(String:Native) #> ()
~~~

Logs a message. [init-logger](#initLogger) must be called before use of _log_.

Example:

~~~
>(init-logger "test-log.log")
[]
>(log "test message")
[]
~~~

#### <a name="toStr">to-str</a>
Syntax:

~~~
(to-str <expr>)
~~~

Type Signature:

~~~
(A) #> String:Native
~~~

Returns readable representation of its argument.

Example:

~~~
>(to-str 42)
"42"
>(to-str (construct List Native))
"[]"
~~~

#### <a name="readFile">read-file</a>
Syntax:

~~~
(read-file <filename>)
~~~
Where:

* _filename_ evaluates to string with path to file

Type Signature:

~~~
(String:Native) #> String:Native
~~~

Reads contents of file specified by _filename_ and returns it as string.

Example:

~~~
>(read-file "foo.txt")
"foo bar baz"
~~~

#### <a name="strSplit">str-split</a>
Syntax:

~~~
(str-split <string> <by>)
~~~
Where:

* _string_ evaluates to string
* _by_ evaluates to string

Type Signature:

~~~
(String:Native String:Native) #> List:Native
~~~

Splits _string_ by _by_ into a List:Native.

Example:

~~~
>(str-split "foo;bar;baz" ";")
["foo" ["bar" ["baz" []]]]
~~~

#### <a name=parseInt>parse-int</a>
Syntax:

~~~
(parse-int <string>)
~~~
Where:

* _string_ evaluates to string

Type Signature:

~~~
(String:Native) #> Int:Native
~~~

Tries to parse string into integer. Throws error if string cannot be parsed.

Example:

~~~
>(parse-int "42")
42
~~~

### Language base

Following functions are always available in language. All these functions are (unlike operators) implemented directly in Velka.

* [foldl-list-native](#foldlListNative)
* [foldr-list-native](#foldrListNative)
* [head-list-native](#headListNative)
* [is-list-native-empty](#isListNativeEmpty)
* [map-list-native](#mapListNative)
* [map2-list-native](#map2ListNative)
* [tail-list-native](#tailListNative)

#### <a name="foldlListNative">foldl-list-native</a>

Syntax:

~~~
(foldl-list-native <function> <terminator> <list>)
~~~
Type signature:

~~~
(((A A) #> A) A List:Native) #> A
~~~

Where:

* _function is function of type _(A A) #> A
* _terminator_ is value of type _A_
* _list_ is value of type _List:Native_, containing values of type _A_

_foldl-list-native_ aggregates given _list_ from left to right with _function_ using _terminal_ as first value to base aggregation upon.

Example:

~~~
(foldl-list-native 
    +
    0
    (construct List Native 1 
        (construct List Native 2 
            (construct List Native)))) ; = 3
    
(foldl-list-native
    (lambda (x y) y)
    "a"
    (construct List Native "b" 
        (construct List Native "c" 
            (construct List Native))))  ; = "c"
~~~

#### <a name="foldrListNative">foldr-list-native</a>

Syntax:

~~~
(foldr-list-native <function> <terminator> <list>)
~~~
Type signature:

~~~
(((A A) #> A) A List:Native) #> A
~~~

Where:

* _function_ is function of type _(A A) #> A_
* _terminator_ is value of type _A_
* _list_ is value of type _List:Native_, containing values of type _A_

_foldr-list-native_ aggregates given _list_ from right to left with _function_ using _terminal_ as first value to base aggregation upon.

Example:

~~~
(foldr-list-native 
    +
    0
    (construct List Native 1 (
        construct List Native 2 
            (construct List Native)))) ; = 3
    
(foldr-list-native
    (lambda (x y) y)
    "a"
    (construct List Native "b" 
        (construct List Native "c" 
            (construct List Native)))) ; = "b"
~~~

#### <a name="headListNative">head-list-native</a>

Syntax:

~~~
(head-list-native <list>)
~~~
Type Signature:

~~~
(List:Native) #> A
~~~

Returns head (first element) of given _List:Native_. Causes error if used on empty list.

Example:

~~~
(head-list-native 
    (construct List Native 1 
        (construct List Native 2 
            (construct List Native)))) ; = 1
            
(head-list-native (construct List Native)) ; ERROR
~~~

#### <a name="isListNativeEmpty">is-list-native-empty</a>

Syntax:

~~~
(is-list-native-empty <list>)
~~~

Type Signature:

~~~
(List:Native) #> Bool:Native
~~~

Returns _true_ if given _list_ is an empty list, returns _false_ otherwise.

Examples:

~~~
(is-list-native-empty 
    (construct List Native 1 
        (construct List Native))) ; = false
        
(is-list-native-empty (construct List Native)) ; = true
~~~

#### <a name="mapListNative">map-list-native</a>
Syntax:

~~~
(map-list-native <function> <list>)
~~~
Type Signature:

~~~
(((A) #> B) List:Native) #> List:Native
~~~
Where:

* _function_ is function with type signature _(A) #> B_
* _list_ is _List:Native_ containing values of type _A_

Maps _function_ to each element of _list_ and returns list containing the results. Plese note the elements of resulting list will be of type _B_.

Example:

~~~
(map-list-native
    (lambda (x) (+ x 1))
    (construct List Native 1 (
        construct List Native 2 
            (construct List Native)))) ; = list containing (2 3)
~~~

#### <a name="map2ListNative">map2-list-native</a>
Syntax:

~~~
(map2-list-native <function> <list-1> <list-2>)
~~~
Type Signature:

~~~
(((A B) #> C) List:Native List:Native) #> List:Native
~~~

Where:

* _function_ is function with type signature _(A B) #> C_
* _list-1_ is _List:Native_ with elements of type _A_
* _list-2_ is _List:Native_ with elements of type _B_
* _list-1_ and _list-2_ must be of same length

Maps _function_ to each element of list _list-1_ and _list-2_ and returns _List:Native_ containign results. Please note the result will be _List:Native_ containing elements of type _C_. Throws error if lists are not of the same length.

Example:

~~~
(map2-list-native
    +
    (construct List Native 1 
        (construct List Native 2 
            (construct List Native)))
    (construct List Native 9 
        (construct List Native 8 
            (construct List Native)))) ; = List containing (10 10)
~~~

#### <a name="tailListNative">tail-list-native</a>
Syntax:

~~~
(tail-list-native <list>)
~~~
Type Signature:

~~~
(List:Native) #> List:Native
~~~

Returns tail (remainder of list after the first element) of given _list_. Throws an error if used with empty list.

Example:

~~~
(tail-list-native 
    (construct List Native 1 
        (construct List Native 2 
            (construct List Native)))) ; = list containing (2)
            
(tail-list-native (construct List Native)) ; ERROR
~~~

### <a name="typeSignature">Type signatures</a>
There are three kind of type signatures present in language: atomic types, composite types and type variables

#### Atomic Types
Atomic types in language are described in two ways:

    <TypeName>
    <TypeName>:<RepresentationName>
    
Where former style describes value of <TypeName> type in any representation and is equal to <TypeName>:* in latter signature style.

For example signature of integer represented by string of roman number would be:

    Int:Roman
    
For example signature of general double type (without specific representation) would be:

    Double

or

    Double:*

#### Composite types
There are two kind of composite types in language: type tuples and arrow types (types of functions).

For type tuples we use list syntax:

    (<subtype>*)
    
For example 2-tuple containing native integer and native string would be:

    (Int:Native String:Native)
    
Arrow types are types of functions, they use following syntax:

    <formal argument type> #> <return type>
    
Also note that type of formal argument is ALWAYS a type tuple, even for functions only accepting one argument.
    
For example function that accept tvo native integers and returns native string has following signature:

    (Int:Native Int:Native) #> String:Native
    
Composite types can be arbitrarily nested.

#### Type variables
In order to allow generic types (like generic list), type variables can be intorduced into type signatures.

Type variable is a type that can be unified with arbitrary type, but when it is unified it stays assigned to that type.

For example, lets have following identity function:

~~~
(lambda (x) x)
~~~

This function has type of `(A) #> A` where `A` is a type variable. This allows funtion to accept argument of any type and return value of the same type. Howerver aaplication of this function forces unification of this type variable, so for example:

~~~
((lambda (x) x) 42)
~~~

Is of type `Int:Native` and during the inference type variable `A` is unified with `Int:Native` substituing type of the function to `(Int:Native) #> Int:Native`.

If we wanted to specify name for the type variable explicitely we can use:

~~~
(let-type (A) (lambda ((A x)) x))
~~~

Where `let-type` is special form that declares and specifies scope of this type variable. For details on syntax see [let-type](#let-type). Undeclared type variables are not allowed.

### Lambda expression with type signatures
Lambda expression in language supports type signatures on formal arguments of lambda expression. For example lambda that accepts only String arguments:

    (lambda ((String x)) x)
    
You can also use type signature with representation specified. For example lambda that only accepts Native representation of String:

    (lambda ((String:Native x)) x)
    
For lambda expressions type signatures are strictly not-mandatory.

### Build-in Types
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
    
### Types, Representations and Constructors
To define a new type use _type_ special form. For example to define a type called Name:

    (type Name)
    
Type cannot be instantiated without representation and constructor. To define representation use _representation_ special form. Each type can have multiple representations. For expample to create two representations of type Name called Structured and Unstructured.

    (representation Structured Name)
    (representation Unstructured Name)
    
Representation cannot be instantiated without constructor. Any representation can have multiple constructors distinguished by number of arguments. This is due to requirement for each representation to be able to represent all values any value of given type. To define constructor use _constructor_ special form. For example to define constructors for Name:Structured representation and Name:Unstructured representation.

    (constructor Name Structured 
        ((String:Native firstName) (String:Native secondName)) 
            (cons firstName secondName))
    (constructor Name Unstructured ((String:Native name)) name)
    
### Instantiating types, conversions and extended-lambda
To instantiate representation of given type use _construct_ special form. For example to instantiate representations from previous section.

    (construct Name Structured "John" "Doe") => [John Doe]
    (construct Name Unstructured "John Doe") => John Doe
    
Language supports and uses conversions between individual representations. To define conversion between representations use _conversion_ special form. For example to define conversion from Name:Structured to Name:Unstructured representation:

    (conversion Name:Structured Name:Unstructured 
        ((Name:Structured x) 
            (construct Name Unstructured (concat 
                (car (deconstruct x (String:Native String:Native))) 
                (cdr (deconstruct x (String:Native String:Native)))))))
    
To explicitely invoke conversion use _convert_ special form. For example:

    (convert Name:Structured Name:Unstructured 
        (construct Name Structured "John" "Doe")) => JohnDoe
    
However conversions are mostly invoked implicitely during evaluation of functiona application. For example lets have lambda that only accepts Name:Unstructured argument:

    ((lambda ((Name:Unstructured x)) 
            (concat (deconstruct x) "0")) 
        (construct Name Structured "John" "Doe")) => JohnDoe0
    
System tries to convert any arguments to required representations and then applies function to converted arguments. If conversion does not exists error is thrown.

Extended-lambda is special form, allowing us to create functions which can behave specifically to representations of its arguments. For example:

    (define f (extended-lambda ((Name x)) 
                                ((Name:Structured) "Structured")
                                ((Name:Unstructured) "Unstructured")))
                                
    (f (construct Name Structured "John" "Doe")) => Structured
    (f (construct Name Unstructured "John Doe")) => Unstructured
    
#### <a name="typeSymbol">Type symbols</a>
**THIS IS SUBJECT TO CHANGE AND MIGHT BE RENDERED OBSOLETE IN FUTURE RELEASES**

Some build in operators are working with so called type symbols. Type symbol is a special value, that is of given _type_ and also holds that type as a value. Its usage is currently only in [cost functions](#rankingFunction). They cannot be explicitely created, nor should be used outside of cost functions.

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

#### Extended function application
Extended function can have multiple different implementations depending on representations of arguments it is applied to. They are created by special form [_extended-lambda_](#extendedLambda) or [_extended-lambda-ranking_](#extendedLambdaRanking). For example:

~~~
(def foo (extended-lambda ((Int x))
    ((Int:Native) "This is Int:Native")
    ((Int:String) "This is Int:String"))
    
(foo 42) ; = "This is Int:Native"
(foo (construct Int String "42")) ; = "This is Int:String"
~~~

Extended functions are usefull when dealing with data that can come in various different implementation. Extended functions allow us to use specific algorithms for specific representations of data. For example binary matrix can be represented by two dimensional array, bit array or incidence list. Extended functions allow us to tranparently implement functions using efficient algorithms based on representations of arguments.

##### Implementation selection
When extended function is applied, it is using auxiliary *cost function* to rank and select implementation that will be used. Ranking function maps each implementation along with arguments of application to integer. Then implementation with least rank is selected and applied.

If no cost function is specified, default cost function is used. Default cost function compares representations of supplied arguments and formal arguments of implementations and increases cost by _1_ for each representation that is different. Therefore if supplied arguments have exactly same representation as formal arguments of implementations default cost function will return _0_. On the other hand if representation of each supplied argument and formal argument of implementation is different, default cost function will return number equal to number of arguments.

##### <a name="rankingFunction">Cost function</a>
Cost function is any function or extended-function with following type signature:

~~~
(List:Native List:Native A) #> Int:Native
~~~

During each application of extended function cost function is called for each implementation of that extended function. First argument passed to cost function is _List:Native_ of [type symbols](#typeSymbol) containing types of arguments implementation is expecting, second argument is list of [type symbols](#typeSymbol) containing types of arguments that extended function is applied to and final argumet is tuple of evaluated arguments of the application. For example following expression:

~~~
((extended-lambda 
        ((Int x)) 
        ((Int:Native) "Native")) 
    (construct Int Roman "XLII"))
~~~

Will result in application of cost function roughly equivalent to following pseudo-expression:

~~~
(ranking-function
    ((type-symbol Int:Native))
    ((type-symbol Int:Roman))
    (tuple <"XLII" Int:Roman>))
~~~

Such application would be evaluated for each implementation of applied extended function (above example has only one implementation).

Default cost funtion has roughly following implementation:

~~~
(lambda ((List:Native formalArgTypes) (List:Native realArgs) (A args))
    (foldr-list-native + 0 (map2-list-native 
                            (lambda (x y) (if (is-same-representation x y) 0 1)) 
                            formalArgTypes
                            realArgs)))
~~~

##### Specifing cost function
To specify cost function we are using special forms [_extended-lambda-ranking_](#extendedLambdaRanking) and [_eapply_](#eapply). Latter allows us to specify ranking function for specific application of any extended function (or even function but for them cost function would be ignored).

Special form [_extended-lambda-ranking_](#extendedLambdaRanking) allows us to associate specific cost function with defined extended function. Then, any time this extended function is applied, its associated cost function is used for implementation selection, unless [_eapply_](#eapply) with different ranking function is used.

Therefore priority in which specific cost functions are used is following:

1. cost function used with [_eapply_](#eapply) special form
2. cost function associated with extended function by [_extended-lambda-ranking_](#extendedLambdaRanking) special form
3. default cost function

Following example, shows usage of both ways to specify cost function:

~~~
(def cost-prefer-int-roman
    (lambda ((List:Native formal-arg-types) (List:Native real-arg-types) (A args))
                (if (instance-of-representation 
                        (head-list formal-arg-types) 
                        Int:Roman)
                    0
                    999)))
                    
(def cost-prefer-int-string 
    (lambda ((List:Native formal-arg-types) (List:Native real-arg-types) (A args))
                (if (instance-of-representation 
                        (head-list formal-arg-types) 
                        Int:String)
                    0
                    999)))

(def no-cost-fn (extended-lambda ((Int x) (Int y)) 
                    ((Int:Native Int:Native) "Int:Native")
                    ((Int:Roman Int:Native) "Int:Roman")
                    ((Int:String Int:Native) "Int:String"))

(def cost-fn (extended-lambda-ranking 
                    ((Int x) (Int y))  
                    cost-prefer-int-roman
                    ((Int:Native Int:Native) "Int:Native")
                    ((Int:Roman Int:Native) "Int:Roman")
                    ((Int:String Int:Native) "Int:String"))

;; Use default ranking
(no-cost-fn 42 42)                                   ; = "Int:Native"

;; Use ranking specified in function
(cost-fn 42 42)                                      ; = "Int:Roman"

;; Use ranking specified by eapply
(eapply cost-fn (cons 42 42) cost-prefer-int-string) ; = "Int:String"
~~~
    
[antlr4]: https://www.antlr.org/download.html
[junit5]: https://search.maven.org/search?q=g:org.junit.jupiter%20AND%20v:5.6.1;
[apiguardian]: https://github.com/apiguardian-team/apiguardian;
