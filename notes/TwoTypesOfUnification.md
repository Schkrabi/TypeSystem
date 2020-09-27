# Two types of unification

TODO: I am using world isomorphism in a incorrect way here, I am researching for correct term and will change it.

Introducing type representations to type system changes unification algorithm. Where I had extended notion of equality before now the intention and usage of algorithm somewhat splits. I have two (seemingly?) contradictory usages of unification algorithm:

1. Unification for typechecking and inference
2. Unification as extended equality of types up to isomorphism

## Unification for typechecking and inference

This is original intention of unification algorithm. To see if two (potentialy composed) types are compatible for use of its value.

However this is also the intention that changed with introduction of type representation. Whereas originally types like `Int -> A` and `Int -> B` would unify without issue, what should be behavior for example for pair of following types?

~~~
Int:Native -> A
Int:Roman -> B
~~~

Remining of the original semantic in type inference, answer is yes, those types should unify because they have same base types and there exists mapping (biection) between their type variables.

However, you can note that this semantic basically destroys unification as equality on the representation level. So types from previous example are the same, but they do not have the same representation.

## Unification as extended equality of types up to isomorphism

Language needs notion of type equality. If I ommit previous usage in type inference and type checking, there is still purely tehncal requirement for non instance dependant equality (like default implementation of object equality in Java).

Of course there exists staightforward piecewise equality, recursively crawling structure of types down to atomic values. This is fine for most technical requirements, however it lacks taking isomorphism of type variables into account.

My original motivation was to select implementation of extended lambda based on lexicographical distance of type representation (type representation tuple) of arguments and type representation (type representation tuple) of formal argument. So for example:

~~~
(define foo (extended-lambda ((Int a) (Int b))
                ((Int:Native Int:Native) (+ a b))
                ((Int:Native Int:String) (+ a (IntString2IntNative b)))
                ((Int:String Int:String) (construct Int String (concat (deconstruct a String:Native) (deconstrut b String:Native))))))
                
(foo 42 42)
(foo 42 (construct Int String "42"))
(foo (construct Int String 42) (construct Int String 42))
~~~

So in first application of `foo`, first implementation should be selectd, because representations of arguments and formal arguments fits perfectly. Second and third application should yield second and third application respectively for the same reason.

Since I cannot use hard-equality for this (isomorphism of type variables would not be taken into account), logical choice is use unification for this task. However this will not work.

If I use semantic of unification described in previous section, all lexicographical distances in example would yield `0` and one of the implementations would be selected in unspecified way. (Due tu implementation of the language it would be the first one) This is because all the types of formal arguments of implementation (and all the types of arguments too) are the same on type level. Difference is on level of type representation.

Unification semantics as described in previous section are isomorphic with respect to types, but they are also isomorphic with repect to type representations. While in the example I need unification to be isomorphic only with respect to variables but not with respect to type representations.

This leads me to split unification algorithms into two separate algorithms:

1. `Unify-types` which has semantic described in previous section
2. `Unify-representations` which has semantic described in this section

