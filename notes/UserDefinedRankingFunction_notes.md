# User defined ranking function - notes
Target code in language:

~~~
(define my-ranking (lambda (formalArgTypes realArgs)
                    (reduce + (map-2 
                                (lambda (x y) (if (is-same-representation x y) 0 1))
                                formalArgTypes
                                realArgs) 0)))
                                
(apply-with-ranking 
    (extended-lambda ((Int x))
        ((Int:Native) "Native")
        ((Int:String) "String")
        ((Int:Roman) "Roman"))
    (construct List Native 42 (construct List Native))
    my-ranking)
    
(define extended-lambda-with-ranking
    (extended-lambda-with-ranking
        my-ranking
        ((Int x))
            ((Int:Native) "Native")
            ((Int:String) "String")
            ((Int:Roman) "Roman")))
~~~

Where:

* _formalArgTypes_ is list carrying types and representations of formal arguments of ranked implementation
  * it carries types and representations in special symbols (literals?) carrying types
    * it iterprets onto itself
    * it infers to carried type and empty substitution
    * in clojure it will be some small value, that can carry metadata (probably empty vector [])
* _realArgs_ is list with (already evaluated) arguments
* _(is-same-representation val1 val2)_
  * compares types and representation of two values and returns true if they are equal (unifies?)
  * otherwise returns false
  * operator might be sufficient to implement this

## Additional stuff
_(is-same-type val1 val2)_

* similar to _is-same-representation_ but does not take representations of types into account

_(instanceof-representation value type)_

* special form, that infers type and representation of _value_ and compares it (tries to unify?) with _type_

_(instanceof value type)_

* special form, that infers type of _value_ and tries to unify it with _type_

## TODO
* special symbols carrying type
* _List:Native_ utility functions, _map_, _map-2_ and _reduce_
* unificationa and type comparison in clojure
* _instanceof_ and _instanceof-representation_
* _is-same-representation_ and _is-same-type_
* implement new ranking function in clojure
* modify _eapply_ in clojure to reflect new argument type
* _abstractionApplication_ with customable ranking function
* ranking function in _extended-lambda_
