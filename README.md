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
    and
    cons
    construct
    convert
    constructor
    conversion
    extended-lambda
    lambda
    representation
    type
    define
    type
    error
    if
    or

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
There are two kind of type signatures present in language: atomic types and composite types

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
