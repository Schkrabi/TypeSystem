# Type System
## Manual
### Compilation
#### Prerequisities
For compilation you will need following libraries

* **antlr4**
 You can get it [here][antlr4]. Version used during development is antlr-4.7.2-complete.
* **junit5** 
 You will need jupiter.api library you can get sources from [here][junit5]. Version used during development was org.junit.jupiter.api_5.5.1.v20190826-0900.
* **apiguardian**
 Not strictly necessary, will prevent some false warnings during build, git-hub [here][apiguardian]. Version used during developemnt was org.apiguardian_1.1.0.v20190826-0900.
    
#### Ant properties
Before compilation, set following properties in ant script Ant/build.xml

* **antlr.lib**
 location of antlr4 library
* **jupiter.lib.api**
 location of junit5 library
* **apiguardian.lib** 
 location of apiguardian library
    
#### Ant targets
Ant script in Ant/build.xml has following targets

* **clean**
 deletes build forlder and all its contents
* **compile**
 compiles all sources to .class files
* **jar**
 creates executable jar in build/jar/
* **run**
 runs the compiler
* **main**
 cleans build and runs the compiler

### Usage

Program can be used in 3 general modes: as interpreter, as interpreter with preloaded definitions and as compiler to clojure code. Used mode is dependent on number of arguments provided.

#### Interpreter
To use interpreter simply start program without arguments:

    >java -jar compiler.jar
    
In interpreter mode you can evaluate expressions and modify top level environment and type environment (see "define", "type", "representation", "constructor", "conversion").

#### Interpretr with loaded definitions
To use interpreter with loaded definitions supply file with code as first argument.

    >java -jar compiler.jar definitions.src
    
This mode works in the same way as interpreter. Only difference is that expressions from supplied file is evaluated before running interpretter loop.

#### Compiler
To use compiler to clojure code, supply two arguments. First argument is input file with code, second file is output file with compiled clojure code.

    >java -jar compiler.jar input.src output.clj
    
Output file is standalone and is ready to be compiled by clojure compilator.

### Language
Language has no name so far.

Language is similar to Scheme, (so far) without macro support and with limited set of build-in functions and operators. 

Weight of the language lies in its type system, separated type and representation definitions and usage of extended-lambda.

#### Special forms
Here is reference to all special form present in language.

	and
	can-deconstruct-as
    cons
    construct
    convert
    constructor
    conversion
	deconstruct
	define
	error
    extended-lambda
	if
    lambda
    let-type
	or
    representation
    type

##### _and_
Syntax:
~~~
	(and <arg1> <arg2>
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

##### _can-deconstruct-as
Syntax:
~~~
	(can-deconstruct-as <expression> <type-signature>)
~~~
Where:
* _<expression>_ expresion which is evaluated and its value can be deconstructed as a specified type
* _<type-signature>_ [type signature](TODO)

Checks if value in the expression can be deconstructed as specified type. Returns _Bool:Native_.

Example:
~~~
	(can-deconstruct-as (construct Name Structured "Jane" "Doe") (String:Native String:Native))
~~~

##### _cons_
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
	(cons 42 "fooL)
~~~

##### _construct_
Syntax:
~~~
	(construct <type> <representation> <arg1>...)
~~~

Constructs value of given type and representation based on previously defined constructor. Also see [constructor](TODO)

Example:
~~~
	(construct Int Native 42)
	(construct Name Structured "Jane" "Doe")
~~~

##### _convert_
Syntax:
~~~
	(convert <from representation> <to representation> <value>)
~~~

Converts its argument into another type representation if such conversion was previously defined. Also see [conversion](TODO)

Example:
~~~
	(convert Int:Native Int:Roman 42)
~~~

##### _constructor_
Syntax:
~~~
	(constructor <type> <representation> (<constructor arguments>...) <constructor body>)
~~~

Defines constructor for given type and representation

Example:
~~~
	(constructor Name Structured ((String:Native first) (String:Native second)) (cons first second))
~~~

##### _conversion_
Syntax:
~~~
	(conversion <from representation> <to representation> (<conversion argument>) <conversion body>)
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

##### _deconstruct_
Syntax:
~~~
	(deconstruct <expression> <type-signature>)
~~~
Where:
* _<expression>_ is expression which will be evaluated and deconstructed to given type. 
* _<type-signature>_ [type signature](TODO)

This special form attemps to deconstruct given value to a given type. Due to compiler limitations there is no check at compile time if the deconstruction is correct and user should use special form [_can-deconstruct-as](TODO) to check the deconstruction at runtime.

Example:
~~~
	(deconstruct (construct Int Roman "XLII") String:Native)
~~~

##### _define_
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

##### _error_
Syntax:
~~~
	(error <expression>)
~~~
Where:
* _<expression>_ is a expression that yields _String_ and is contained in error message.

Throws user defined exception.

Example:
~~~
	(define safe-div
		(lamdba (x y)
			(if (= y 0)
				(error "Error, division by zero.")
				(/ x y))))
~~~

##### _extended-lambda_
Syntax:
~~~
	(extended-lambda (<argument-list>) ((<argument-representation list>) <implementation>)...)
~~~
Where:
* _<argument-list>_ is list of typed arguments without specified representations
* _<argument-representation list>_ is list of argument representations used for a specific implementation
* _<implementation>_ is a body of function for specific representations of arguments

Creates extended function that is either directly applicable or can be bound to symbol via _define_.

Example:
~~~
	(extended-lambda ((Int x))
		((Int:Native) (+ x 1))
		((Int:String) (construct Int String (concat "1" (deconstruct x String:Native)))))
~~~

##### _if_
Sytax:
~~~
	(if <condition> <true-branch> <false-branch>)
~~~
Where:
* _<condition>_ is an expression that yields _Bool_
* _<true-branch>_ is an expression that will be evaluated if _<condition>_ yields _true_
* _<false-branch>_ is an expression that will be evaluated if _<codition>_ yields _false_

Traditional branchinch expression, _<condition>_ is always evaluated, if it yields _true_ _<true-branch>_ is evaluated, otherwise _<false-branch>_ is evaluated.

~~~
	(lambda (x)
		(if (= x 0)
			"zero"
			"non-zero"))
~~~

##### _lambda_
Syntax:
~~~
	(lambda (<argument-list>) <body>)
~~~
Where:
* _<argument-list>_ is a list of either typed or untyped arguments
* _<body>_ is expression evaluated when the function is applied

Creates a function.

Examples:
~~~
	(lambda (x) x)
	(lambda ((Int x) (Int y)) (+ x y))
	(lambda ((Name:Structured name)) (car (deconstruct name (String:Native String:Native))))
	(lambda (value (List l)) (append-list value l))
~~~

##### _let-type_
Syntax:
~~~
	(let-type (<list-of-type-variables>) <body>)
~~~
Where:
* _<list-of-type-variables>_ is list of declared type variables
* _<body>_ is expression that is evaluated within scope of type variables

Declares number of type variables and defines its scope. In _<body>_ you can freely use type variables defined in _let-type_

Example:
~~~
	(let-type (A)
		(lambda ((A x)) x))
~~~

##### _or_
Syntax:
~~~
	(or <arg1> <arg2>
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

##### _representation_
Syntax:
~~~
	(representation <representation-name> <type-name>)
~~~
Where:
* _<representation-name>_ is name of defined representation
* _<type-name>_ is name of already defined type

Defines new type representation to already defined type. See [type](TODO)

Example:
~~~
	(representation Structured Name)
~~~

##### _type_
Syntax:
~~~
	(type <type-name>)
~~~

Defines new type.

Examples:
~~~
	(type Name)
~~~

#### Operators
    +
    bit-and
    bit-or
    car
    cdr
    concat
    /
    equals?
    <
    *
    not
    =
    -
    println
    deconstruct
    IntNative2IntString
    IntNative2IntRoman
    IntString2IntNative
    IntString2IntRoman
    IntRoman2IntNative
    IntRoman2IntString
    
#### Type signatures
There are three kind of type signatures present in language: atomic types, composite types and type variables

##### Atomic Types
Atomic types in language are described in two ways:

    <TypeName>
    <TypeName>:<RepresentationName>
    
Where former style describes value of <TypeName> type in any representation and is equal to <TypeName>:* in latter signature style.

For example signature of integer represented by string of roman number would be:

    Int:Roman
    
For example signature of general double type (without specific representation) would be:

    Int

or

    Int:*

##### Composite types
There are two kind of composite types in language: type tuples and arrow types (functiona types).

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

##### Type variables
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

Is of type `Int:Native` and during the inference type variable `A` is unfied with `Int:Native` substituing type of the function to `Int:Native #> Int:Native`.

If we wanted to specify name for the type variable explicitely we can use:

~~~
    (let-type (A) (lambda ((A x)) x))
~~~

Where `let-type` is special form that declares and specifies scope of this type variable. For details on syntax see [let-type](TODO). Undeclared type variables are not allowed.


#### Lambda expression with type signatures
Lambda expression in language supports type signatures on formal arguments of lambda expression. For example lambda that accepts only String arguments:

    (lambda ((String x)) x)
    
You can also use type signature with representation specified. For example lambda that only accepts Native representation of String:

    (lambda ((String:Native x)) x)
    
For lambda expressions type signatures are strictly not-mandatory.

#### Build-in Types
Following types and their representations are build-in to compiler:

| Type   | Representation | Note                                            |
|--------|----------------|-------------------------------------------------|
| Int    | Native         |                                                 |
| Int    | String         | Integer represented by decimal string           |
| Int    | Roman          | Integer represented by string with roman number |
| String | Native         |                                                 |
| Double | Native         |                                                 |
| Bool   | Native         |                                                 |
    
#### Types, Representations and Constructors
To define a new type use _type_ special form. For example to define a type called Name:

    (type Name)
    
Type cannot be instantiated without representation and constructor. To define representation use _representation_ special form. Each type can have multiple representations. For expample to create two representations of type Name called Structured and Unstructured.

    (representation Structured Name)
    (representation Unstructured Name)
    
Representation cannot be instantiated without constructor. Any representation can have multiple constructors. This is due to requirement for each representation to be able to represent all values any value of given type. To define constructor use _constructor_ special form. For example to define constructors for Name:Structured representation and Name:Unstructured representation.

    (constructor Name Structured 
        ((String:Native firstName) (String:Native secondName)) 
            (cons firstName secondName))
    (constructor Name Unstructured ((String:Native name)) name)
    
#### Instantiating types, conversions and extended-lambda
To instantiate representation of given type use _construct_ special form. For example to instantiate representations from previous section.

    (construct Name Structured "John" "Doe") => [John Doe]
    (construct Name Unstructured "John Doe") => John Doe
    
Language supports and uses conversions between individual representations. To define conversion between representations use _conversion_ special form. For example to define conversion from Name:Structured to Name:Unstructured representation:

    (conversion Name:Structured Name:Unstructured 
        ((Name:Structured x) 
            (construct Name Unstructured (concat 
                (car (deconstruct x)) 
                (cdr (deconstruct x))))))
    
To explicitely invoke conversion use _convert_ special form. For example:

    (convert Name:Structured Name:Unstructured 
        (construct Name Structured "John" "Doe")) => JohnDoe
    
However conversions are mostly invoked implicitely during evaluation of functiona application. For example lets have lambda that only accepts Name:Unstructured argument:

    ((lambda ((Name:Unstructured x)) (concat (deconstruct x) "0")) 
        (construct Name Structured "John" "Doe")) => JohnDoe0
    
System tries to convert any arguments to required representations and then applies function to converted arguments. If conversion does not exists error is thrown.

Extended-lambda is special form, allowing us to create functions which can behave specifically to representations of its arguments. For example:

    (define f (extended-lambda ((Name x)) 
                                ((Name:Structured) "Structured")
                                ((Name:Unstructured) "Unstructured")))
                                
    (f (construct Name Structured "John" "Doe")) => Structured
    (f (construct Name Unstructured "John Doe")) => Unstructured
    
    
[antlr4]: https://www.antlr.org/download.html
[junit5]: https://search.maven.org/search?q=g:org.junit.jupiter%20AND%20v:5.6.1;
[apiguardian]: https://github.com/apiguardian-team/apiguardian;
