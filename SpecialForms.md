# Special forms
Here is reference to all special form present in language.

## Table of Contents
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
* [extend](#extend)
* [extended-lambda](#extendedLambda)
* [get](#get)
* [if](#if)	
* [instance-of](#instanceOf)
* [instance-of-representation](#instanceOfRepresentation)
* [lambda](#lambda)
* [let](#let)
* [let*](#let-ast)
* [let-type](#letType)
* [loop](#loop)
* [or](#or)
* [recur](#recur)
* [representation](#representation)
* [tuple](#tuple)
* [type](#type)

### <a name="and">and</a>
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

### <a name="canDeconstructAs">can-deconstruct-as</a>
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

### <a name="cons">cons</a>

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

### <a name="construct">construct</a>
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

### <a name="convert">convert</a>
Syntax:

~~~
(convert <from-representation> <to-representation> <value>)
~~~

Converts its argument into another type representation if such conversion was previously defined. Also see [conversion](#conversion)

Example:

~~~
(convert Int:Native Int:Roman 42)
~~~

### <a name="constructorSpecForm">constructor</a>
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

### <a name="conversion">conversion</a>
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

### <a name="deconstruct">deconstruct</a>
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

### <a name="define">define</a>
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

### <a name="eapply">eapply</a>
Syntax:

~~~
(eapply <abstraction> <arguments>)
~~~

Where:

* _abstraction_ is an expression that evaluates into function or extended-function
* _arguments_ is an exprpression that evaluates into tuple of arguments

This is application special form, it allows application of function or extended function with arguments in a tuple

Example:

~~~
(eapply (lambda (x y) x) (cons 42 "foo"))
~~~

### <a name="error">error</a>
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

### <a name="extend">extend</a>
Syntax:

~~~
(extend <extended-function> <extending-implementation> [<const-function>])
~~~
Where:

* _extended-function_ is an expression that evaluates into an extended function
* _extending-implementation_ is an expression that evaluates into implementation viable for extended function
* _cost-function_ non mandatory, it is an expression that evaluates into cost fuction for the implementation.

Adds new implementation to extended function.

Example:

~~~
(define foo (extended-lambda ((Int))))
(define foo (extend foo (lambda ((Int:String)) "bar")))
(foo (construnct Int String "42")) => "bar"
~~~

### <a name="extendedLambda">extended-lambda</a>
Syntax:

~~~
(extended-lambda 
    (<argument-type-list>))
~~~
Where:

* _argument-type-list_ is list of argument _types_ used to verify argument representations of extends


Creates anonymous extended funtion without implementations.

Example:

~~~
(extend (extend 
	(extended-lambda ((Int)))
	(lambda ((Int:Native x)) (+ x 1)))
	(lambda ((Int:String x)) (construct Int	String 
								(concat 
									"1" 
									(deconstruct x String:Native)))))
~~~

### <a name="get">get</a>
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

### <a name="if">if</a>
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

### <a name="instanceOf">instance-of</a>
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

### <a name="instanceOfRepresentation">instance-of-representation</a>
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

### <a name="lambda">lambda</a>
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

### <a name="let">let</a>
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

### <a name="let-ast">let*</a>
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


### <a name="letType">let-type</a>
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

### <a name="or">or</a>
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

### <a name="representation">representation</a>
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

### <a name="tuple">tuple</a>
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

### <a name="type">type</a>
Syntax:

~~~
(type <type-name>)
~~~

Defines new type.

Examples:

~~~
(type Name)
~~~