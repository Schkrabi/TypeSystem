# List
Operators for working with velka Lists

## Table of Contents
* [constructorEmpty(velka.core.langbase.ListNative$1@2df32bf7)](#velka.core.langbase.ListNative$1)
* [constructor(velka.core.langbase.ListNative$2@530612ba)](#velka.core.langbase.ListNative$2)
* [isEmpty(is-list-native-empty)](#velka.core.langbase.ListNative$3)
* [headListNativeOperator(head-list-native)](#velka.core.langbase.ListNative$4)
* [tailListNativeOperator(tail-list-native)](#velka.core.langbase.ListNative$5)
* [mapListNativeOperator(map-list-native)](#velka.core.langbase.ListNative$6)
* [map2ListNativeOperator(map2-list-native)](#velka.core.langbase.ListNative$7)
* [foldlListNativeOperator(foldl-list-native)](#velka.core.langbase.ListNative$8)
* [addToEndOperator(add-to-end-list-native)](#velka.core.langbase.ListNative$9)
* [ListNativeToArrayListOperator(list-native-2-array-list)](#velka.core.langbase.ListNative$10)
* [ListNativeToLinkedListOperator(list-native-2-linked-list)](#velka.core.langbase.ListNative$11)
* [contains(contains-list-native)](#velka.core.langbase.ListNative$12)
* [filter(filter-list-native)](#velka.core.langbase.ListNative$13)
* [get(get-list-native)](#velka.core.langbase.ListNative$14)
* [buildList(build-list-native)](#velka.core.langbase.ListNative$15)
* [remove(remove-list-native)](#velka.core.langbase.ListNative$16)
* [size(size-list-native)](#velka.core.langbase.ListNative$17)
* [append(append-list-native)](#velka.core.langbase.ListNative$18)
* [reverse(reverse-list-native)](#velka.core.langbase.ListNative$19)
* [everyp(everyp-list-native)](#velka.core.langbase.ListNative$20)

## Constructors
### <a name="velka.core.langbase.ListNative$1"> constructorEmpty(velka.core.langbase.ListNative$1@2df32bf7)</a>
Syntax:

~~~
(construct List Native)
~~~

Type Signature:

~~~
[] -> List:Native
~~~

Constructs Empty List:Native.
### <a name="velka.core.langbase.ListNative$2"> constructor(velka.core.langbase.ListNative$2@530612ba)</a>
Syntax:

~~~
(construct List Native <element> <list>)
~~~

Type Signature:

~~~
[SYSGENNAMEcdm, List:Native] -> List:Native
~~~

Constructs new List:Native adding element as head to list.

## Operators
### <a name="velka.core.langbase.ListNative$3"> isEmpty(is-list-native-empty)</a>
Syntax:

~~~
(is-list-native-empty <list>)
~~~

Type Signature:

~~~
[List:Native] -> Bool:Native
~~~

Returns _true_ if list is empty. Returns _false_ otherwise.

Example:

~~~
(is-list-native-empty (construct List Native)) ;; = #t
~~~
### <a name="velka.core.langbase.ListNative$4"> headListNativeOperator(head-list-native)</a>
Syntax:

~~~
(head-list-native <list>)
~~~

Type Signature:

~~~
[List:Native] -> SYSGENNAMEcff
~~~

Returns first element in this list.

Example:

~~~
(head-list-native (build-list-native 5 (lambda (x) x))) ;; = 0
~~~
### <a name="velka.core.langbase.ListNative$5"> tailListNativeOperator(tail-list-native)</a>
Syntax:

~~~
(tail-list-native <list>)
~~~

Type Signature:

~~~
[List:Native] -> List:Native
~~~

Returns list consisting of all elements of original list, except the first element.

Example:

~~~
(tail-list-native (build-list-native 5 (lambda (x) x))) ;; = (1 2 3 4)
~~~
### <a name="velka.core.langbase.ListNative$6"> mapListNativeOperator(map-list-native)</a>
Syntax:

~~~
(map-list-native <list> <function>)
~~~

Type Signature:

~~~
[[SYSGENNAMEcgy] -> SYSGENNAMEcgz, List:Native] -> List:Native
~~~

Returns a List:Native consisting of the results of applying the given function to the elements of list.

Example:

~~~
(map-list-native
    (build-list-native 5 (lambda (x)))
    (lambda (y) (* y 2))) ;; = (0 2 4 6 8)
~~~
### <a name="velka.core.langbase.ListNative$7"> map2ListNativeOperator(map2-list-native)</a>
Syntax:

~~~
(map2-list-native <list1> <list2> <function>)
~~~

Type Signature:

~~~
[[SYSGENNAMEchw, SYSGENNAMEchx] -> SYSGENNAMEchy, List:Native, List:Native] -> List:Native
~~~

Returns a List:Native consisting of the results of applying the given function to the elements of list1 and list2.

Example:

~~~
(map2-list-native
    (build-list-native 5 (lambda (x) x))
    (build-list-native 5 (lambda (x) x))
    +) ;; = (0 2 4 6 8)
~~~
### <a name="velka.core.langbase.ListNative$8"> foldlListNativeOperator(foldl-list-native)</a>
Syntax:

~~~
(foldl-list-native <function> <terminator> <list>)
~~~

Type Signature:

~~~
[[SYSGENNAMEciv, SYSGENNAMEciw] -> SYSGENNAMEciv, SYSGENNAMEciv, List:Native] -> SYSGENNAMEciv
~~~

Performs a reduction on the elements of list, using the terminator value and an associative accumulation function, and returns the reduced value. Processes list from the beginning.

Example:

~~~
(foldl-list-native / 0 (build-list-native 3 (lambda (x) (+ x 1)))) ;; = 0.16666666666666666666666666666667
~~~
### <a name="velka.core.langbase.ListNative$9"> addToEndOperator(add-to-end-list-native)</a>
Syntax:

~~~
(add-to-end-list-native <list> <element>)
~~~

Type Signature:

~~~
[List:Native, SYSGENNAMEcjt] -> List:Native
~~~

Creates new list with appended the specified element to the end of list.

Example:

~~~
(add-to-end-list-native (construct List Native) 42) ;; = (42)
~~~
### <a name="velka.core.langbase.ListNative$10"> ListNativeToArrayListOperator(list-native-2-array-list)</a>
Syntax:

~~~
(list-native-2-array-list <list native>)
~~~

Type Signature:

~~~
[List:Native] -> List:JavaArray
~~~

Converts List:Native to List:JavaArray.

Example:

~~~
(list-native-2-array-list (build-list-native 5 (lambda (x) x)))
~~~
### <a name="velka.core.langbase.ListNative$11"> ListNativeToLinkedListOperator(list-native-2-linked-list)</a>
Syntax:

~~~
(list-native-2-linked-list <list native>)
~~~

Type Signature:

~~~
[List:Native] -> List:JavaLinked
~~~

Converts List:Native to List:JavaLinked.

Example:

~~~
(list-native-2-linked-list (build-list-native 5 (lambda (x) x)))
~~~
### <a name="velka.core.langbase.ListNative$12"> contains(contains-list-native)</a>
Syntax:

~~~
(contains-list-native <list> <element>)
~~~

Type Signature:

~~~
[List:Native, SYSGENNAMEcmi] -> Bool:Native
~~~

Returns true if this list contains the specified element.

Example:

~~~
(contains-list-native (build-list-native 3 (lambda (x) x)) 0) ; = #t
(contains-list-native (build-list-native 3 (lambda (x) x)) 5) ; = #f
~~~
### <a name="velka.core.langbase.ListNative$13"> filter(filter-list-native)</a>
Syntax:

~~~
(filter-list-native <list> <predicate>)
~~~

Type Signature:

~~~
[List:Native, [SYSGENNAMEcnf] -> Bool:Native] -> List:Native
~~~

Returns new List:Native containing only those elements of list, for which predicate returns true.

Example:

~~~
(filter-list-native (build-list-native 5 (lambda (x) x)) (lambda (y) (= (mod y 2) 0))) ;; = (0 2 4)
~~~
### <a name="velka.core.langbase.ListNative$14"> get(get-list-native)</a>
Syntax:

~~~
(get-list-native <list> <index>)
~~~

Type Signature:

~~~
[List:Native, Int:Native] -> SYSGENNAMEcoc
~~~

Returns the element at the specified position in this list.

Example:

~~~
(get-list-native (build-list-native 5 (lambda (x) (* 2 x))) 1) ;; = 2
~~~
### <a name="velka.core.langbase.ListNative$15"> buildList(build-list-native)</a>
Syntax:

~~~
(build-list-native <n> <function>)
~~~

Type Signature:

~~~
[Int:Native, [Int:Native] -> SYSGENNAMEcoz] -> List:Native
~~~

Creates a List:Native of n elements by applying function to the integers from 0 to (- n 1) in order.
If lst is the resulting list, then (get-list-native lst i) is the value produced by (function i).

Example:

~~~
(build-list-native 5 (lambda (x) (* x x))) ;; = (0 1 4 9 16)
~~~
### <a name="velka.core.langbase.ListNative$16"> remove(remove-list-native)</a>
Syntax:

~~~
(remove-list-native <list> <element>)
~~~

Type Signature:

~~~
[List:Native, SYSGENNAMEd] -> List:Native
~~~

Removes the first occurrence of the specified element from this list, if it is present.

Example:

~~~
(remove-list-native build-list-native 3 (lambda (x) x)) 1) ;; = (0 2)
~~~
### <a name="velka.core.langbase.ListNative$17"> size(size-list-native)</a>
Syntax:

~~~
(size-list-native <list>)
~~~

Type Signature:

~~~
[List:Native] -> Int:Native
~~~

Returns the number of elements in this list.

Example:

~~~
(size-list-native (build-list-native 3 (lambda (x) x))) ;; = 3
~~~
### <a name="velka.core.langbase.ListNative$18"> append(append-list-native)</a>
Syntax:

~~~
(append-list-native <list1> <list2>)
~~~

Type Signature:

~~~
[List:Native, List:Native] -> List:Native
~~~

Creates a new List:Native where contents of list2 are appended after contents of list1.

Example:

~~~
(append-list-native (build-list-native 2 (lambda (x) x)) (build-list-native 3 (lambda (x) (+ x 2)))) ;; = (0 1 2 3 4)
~~~
### <a name="velka.core.langbase.ListNative$19"> reverse(reverse-list-native)</a>
Syntax:

~~~
(reverse-list-native <list>)
~~~

Type Signature:

~~~
[List:Native] -> List:Native
~~~

Creates new List:Native with the same elements as list, but in reversed (last to first) order.

Example:

~~~
(reverse-list-native (build-list-native 5 (lambda (x) x))) ;; = (4 3 2 1 0)
~~~
### <a name="velka.core.langbase.ListNative$20"> everyp(everyp-list-native)</a>
Syntax:

~~~
(everyp-list-native <list> <predicate>)
~~~

Type Signature:

~~~
[List:Native, [SYSGENNAMEctg] -> Bool:Native] -> Bool:Native
~~~

Returns true if every element of this list returns true for the predicate. Otherwise returns false.

Example:

~~~
(everyp-list-native (build-list-native 10 (* 2 x)) (lambda (x) (= (mod x 2) 0))) ;; = #t
(everyp-list-native (build-list-native 10 (* 2 x)) (lambda (x) (= x 1))) ;; = #f
~~~