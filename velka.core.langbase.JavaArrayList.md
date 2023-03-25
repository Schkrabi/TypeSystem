# Array List
Operators for working with wrapped java.util.ArrayList.

## Table of Contents
* [constructor(construct List JavaArray)](#velka.core.langbase.JavaArrayList$1)
* [constructorFromList(construct List JavaArray)](#velka.core.langbase.JavaArrayList$2)
* [constructorCapacity(velka.core.langbase.JavaArrayList$3@5fe5c6f)](#velka.core.langbase.JavaArrayList$3)
* [addToEnd(java-array-list-add-to-end)](#velka.core.langbase.JavaArrayList$4)
* [addToIndex(java-array-list-add-to-index)](#velka.core.langbase.JavaArrayList$5)
* [addAll(java-array-list-add-all)](#velka.core.langbase.JavaArrayList$6)
* [contains(java-array-list-contains)](#velka.core.langbase.JavaArrayList$7)
* [containsAll(java-array-list-contains-all)](#velka.core.langbase.JavaArrayList$8)
* [get(java-array-list-get)](#velka.core.langbase.JavaArrayList$9)
* [indexOf(java-array-list-index-of)](#velka.core.langbase.JavaArrayList$10)
* [isEmpty(java-array-list-is-empty)](#velka.core.langbase.JavaArrayList$11)
* [lastIndexOf(java-array-list-last-index-of)](#velka.core.langbase.JavaArrayList$12)
* [remove(java-array-list-remove)](#velka.core.langbase.JavaArrayList$13)
* [removeAll(java-array-list-remove-all)](#velka.core.langbase.JavaArrayList$14)
* [retainAll(java-array-list-retain-all)](#velka.core.langbase.JavaArrayList$15)
* [set(java-array-list-set)](#velka.core.langbase.JavaArrayList$16)
* [size(java-array-list-size)](#velka.core.langbase.JavaArrayList$17)
* [sublist(java-array-list-sublist)](#velka.core.langbase.JavaArrayList$18)
* [map(java-array-list-map)](#velka.core.langbase.JavaArrayList$19)
* [map2(java-array-list-map2)](#velka.core.langbase.JavaArrayList$20)
* [foldl(java-array-list-foldl)](#velka.core.langbase.JavaArrayList$21)
* [foldr(java-array-list-foldr)](#velka.core.langbase.JavaArrayList$22)
* [everyp(java-array-list-everyp)](#velka.core.langbase.JavaArrayList$25)

## Constructors
### <a name="velka.core.langbase.JavaArrayList$1"> constructor(construct List JavaArray)</a>
Syntax:

~~~
(construct List JavaArray)
~~~

Type Signature:

~~~
[] -> List:JavaArray
~~~

Constructs empty List:JavaArray.
### <a name="velka.core.langbase.JavaArrayList$2"> constructorFromList(construct List JavaArray)</a>
Syntax:

~~~
(construct List JavaArray <list>)
~~~

Type Signature:

~~~
[List:Native] -> List:JavaArray
~~~

Construct List:JavaArray from existing list inserting all its elements.
### <a name="velka.core.langbase.JavaArrayList$3"> constructorCapacity(velka.core.langbase.JavaArrayList$3@5fe5c6f)</a>
Syntax:

~~~
(construc List JavaArray <capacity>)
~~~

Type Signature:

~~~
[Int:Native] -> List:JavaArray
~~~

Constructs List:JavaArray with specified pre-allocated capacity.

## Operators
### <a name="velka.core.langbase.JavaArrayList$4"> addToEnd(java-array-list-add-to-end)</a>
Syntax:

~~~
(java-array-list-add-to-end <list> <element>)
~~~

Type Signature:

~~~
[List:JavaArray, SYSGENNAMEbos] -> Bool:Native
~~~

Appends the specified element to the end of list.

Example:

~~~
(java-array-list-add-to-end (construct List JavaArray) 42)
~~~
### <a name="velka.core.langbase.JavaArrayList$5"> addToIndex(java-array-list-add-to-index)</a>
Syntax:

~~~
(java-array-list-to-index <list> <index> <element>)
~~~

Type Signature:

~~~
[List:JavaArray, Int:Native, SYSGENNAMEbpr] -> []
~~~

Inserts the specified element at the specified position in list.

Example:

~~~
(java-array-list-to-index (construct List JavaArray) 0 42)
~~~
### <a name="velka.core.langbase.JavaArrayList$6"> addAll(java-array-list-add-all)</a>
Syntax:

~~~
(java-array-list-add-all <list1> <list2>)
~~~

Type Signature:

~~~
[List:JavaArray, List:JavaArray] -> Bool:Native
~~~

Appends all of the elements in the specified collection to the end of this list, in the order that they are returned by the specified collection's Iterator.

Example:

~~~
(def l (construct List JavaArray))
(java-array-list-add l 42)
(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))
(println l)
;;(42 0 1 2)
~~~
### <a name="velka.core.langbase.JavaArrayList$7"> contains(java-array-list-contains)</a>
Syntax:

~~~
(java-array-list-contains <list> <element>)
~~~

Type Signature:

~~~
[List:JavaArray, SYSGENNAMEbro] -> Bool:Native
~~~

Returns true if this list contains the specified element.

Example:

~~~
(java-array-list-contains (construct List JavaArray) 42) ; = #f
~~~
### <a name="velka.core.langbase.JavaArrayList$8"> containsAll(java-array-list-contains-all)</a>
Syntax:

~~~
(java-array-list-contains-all <list1> <list2>)
~~~

Type Signature:

~~~
[List:JavaArray, List:JavaArray] -> Bool:Native
~~~

Returns true if this list contains all of the elements in the specified list.

Example:

~~~
(def l (construct List JavaArray))
(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))
(java-array-list-contains-all k (build-list-native 2 (lambda (x) x))) ;; = #t
~~~
### <a name="velka.core.langbase.JavaArrayList$9"> get(java-array-list-get)</a>
Syntax:

~~~
(java-array-list-get <list> <index>)
~~~

Type Signature:

~~~
[List:JavaArray, Int:Native] -> SYSGENNAMEbtl
~~~

Returns the element at the specified position in this list.

Example:

~~~
(def l (construct List JavaArray))
(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))
(java-array-list-get l 1) ;; = 1
~~~
### <a name="velka.core.langbase.JavaArrayList$10"> indexOf(java-array-list-index-of)</a>
Syntax:

~~~
(java-array-list-index-of <list> <element>)
~~~

Type Signature:

~~~
[List:JavaArray, SYSGENNAMEbuk] -> Int:Native
~~~

Returns the index of the first occurrence of the specified element in this list, or -1 if this list does not contain the element.

Example:

~~~
(def l (construct List JavaArray))
(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))
(java-array-list-index-of l 1) ;; = 1
~~~
### <a name="velka.core.langbase.JavaArrayList$11"> isEmpty(java-array-list-is-empty)</a>
Syntax:

~~~
(java-array-list-is-empty <list>)
~~~

Type Signature:

~~~
[List:JavaArray] -> Bool:Native
~~~

Returns true if this list contains no elements.

Example:

~~~
(java-array-list-is-empty (construct List JavaArray)) ;; = #t
~~~
### <a name="velka.core.langbase.JavaArrayList$12"> lastIndexOf(java-array-list-last-index-of)</a>
Syntax:

~~~
(java-array-list-last-index-of <list> <element>)
~~~

Type Signature:

~~~
[List:JavaArray, SYSGENNAMEbwh] -> Int:Native
~~~

Returns the index of the last occurrence of the specified element in this list, or -1 if this list does not contain the element.

Example:

~~~
(def l (construct List JavaArray))
(java-array-list-add-all l (build-list-native 3 (lambda (x) 1)))
(java-array-list-last-index-of l 1) ;; = 2
~~~
### <a name="velka.core.langbase.JavaArrayList$13"> remove(java-array-list-remove)</a>
Syntax:

~~~
(java-array-list-remove <list> <element>)
~~~

Type Signature:

~~~
[List:JavaArray, SYSGENNAMEbxg] -> Bool:Native
~~~

Removes the first occurrence of the specified element from this list, if it is present.

Example:

~~~
(def l (construct List JavaArray))
(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))
(java-array-list-remove l 1)
(println l)
(0 2)
~~~
### <a name="velka.core.langbase.JavaArrayList$14"> removeAll(java-array-list-remove-all)</a>
Syntax:

~~~
(java-array-list-remove <list> <element>)
~~~

Type Signature:

~~~
[List:JavaArray, List:JavaArray] -> Bool:Native
~~~

Removes from this list all of its elements that are contained in the specified collection.

Example:

~~~
(def l (construct List JavaArray))
(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))
(java-array-list-add-all l (build-list-native 3 (lambda (x) 1)))
(println l)
(0 1 2 1 1 1)
(java-array-list-remove l 1)
(println l)
(0 2)
~~~
### <a name="velka.core.langbase.JavaArrayList$15"> retainAll(java-array-list-retain-all)</a>
Syntax:

~~~
(java-array-list-retain-all <retained-list> <retainee-list>)
~~~

Type Signature:

~~~
[List:JavaArray, List:JavaArray] -> Bool:Native
~~~

Retains only the elements in this list that are contained in the specified collection.

Example:

~~~
(def l (construct List JavaArray))
(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))
(java-array-list-add-all l (build-list-native 3 (lambda (x) 1)))
(java-array-list-retain-all l (build-list-native 2 (lambda (x) (+ 1 x))))
(println l)
(2 3)
~~~
### <a name="velka.core.langbase.JavaArrayList$16"> set(java-array-list-set)</a>
Syntax:

~~~
(java-array-list-set <list> <index> <element>)
~~~

Type Signature:

~~~
[List:JavaArray, Int:Native, SYSGENNAMEcab] -> SYSGENNAMEcab
~~~

Replaces the element at the specified position in this list with the specified element.

Example:

~~~
(def l (construct List JavaArray))
(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))
(java-array-list-set l 1 42)
(println l)
(0 42 2)
~~~
### <a name="velka.core.langbase.JavaArrayList$17"> size(java-array-list-size)</a>
Syntax:

~~~
(java-array-list-size <list>)
~~~

Type Signature:

~~~
[List:JavaArray] -> Int:Native
~~~

Returns the number of elements in this list.

Example:

~~~
(def l (construct List JavaArray))
(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))
(java-array-list-size l) ;; = 3
~~~
### <a name="velka.core.langbase.JavaArrayList$18"> sublist(java-array-list-sublist)</a>
Syntax:

~~~
(java-array-list-sublist <list> <fromIndex> <toIndex>)
~~~

Type Signature:

~~~
[List:JavaArray, Int:Native, Int:Native] -> List:JavaArray
~~~

Returns a view of the portion of this list between the specified fromIndex, inclusive, and toIndex, exclusive.

Example:

~~~
(def l (construct List JavaArray))
(java-array-list-add-all l (build-list-native 10 (lambda (x) x)))
(java-array-list-sublist l 3 7)
;; = (2 3 4 5 6 7)
~~~
### <a name="velka.core.langbase.JavaArrayList$19"> map(java-array-list-map)</a>
Syntax:

~~~
(java-array-list-map <list> <function>)
~~~

Type Signature:

~~~
[List:JavaArray, [SYSGENNAMEg] -> SYSGENNAMEh] -> List:JavaArray
~~~

Returns a List:JavaArray consisting of the results of applying the given function to the elements of list.

Example:

~~~
(def l (construct List JavaArray))
(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))
(java-array-list-map l (lambda (x) (+ x 2)))
;; = (2 3 4)
~~~
### <a name="velka.core.langbase.JavaArrayList$20"> map2(java-array-list-map2)</a>
Syntax:

~~~
(java-array-list-map2 <list1> <list2> <function>)
~~~

Type Signature:

~~~
[List:JavaArray, List:JavaArray, [SYSGENNAMEi, SYSGENNAMEj] -> SYSGENNAMEk] -> List:JavaArray
~~~

Returns a List:JavaArray consisting of the results of applying the given function to the elements of list1 and list2.

Example:

~~~
(def l1 (construct List JavaArray))
(java-array-list-add-all l1 (build-list-native 3 (lambda (x) x)))
(def l2 (construct List JavaArray))
(java-array-list-add-all l2 (build-list-native 3 (lambda (x) (+ x 1))))
(java-array-list-map2 l1 l2 +)
;; = (1 3 5)
~~~
### <a name="velka.core.langbase.JavaArrayList$21"> foldl(java-array-list-foldl)</a>
Syntax:

~~~
(java-array-list-foldl <function> <terminator> <list>)
~~~

Type Signature:

~~~
[[SYSGENNAMEl, SYSGENNAMEm] -> SYSGENNAMEl, SYSGENNAMEl, List:JavaArray] -> SYSGENNAMEl
~~~

Performs a reduction on the elements of list, using the terminator value and an associative accumulation function, and returns the reduced value. Processes list from the beginning.

Example:

~~~
(def l1 (construct List JavaArray))
(java-array-list-add-all l (build-list-native 3 (lambda (x) (+ x 1))))
(java-array-list-foldl / 0 l) ;; = 0.16666666666666666666666666666667
~~~
### <a name="velka.core.langbase.JavaArrayList$22"> foldr(java-array-list-foldr)</a>
Syntax:

~~~
(java-array-list-foldr <function> <terminator> <list>)
~~~

Type Signature:

~~~
[[SYSGENNAMEn, SYSGENNAMEo] -> SYSGENNAMEn, SYSGENNAMEn, List:JavaArray] -> SYSGENNAMEn
~~~

Performs a reduction on the elements of list, using the terminator value and an associative accumulation function, and returns the reduced value. Processes list from the end.

Example:

~~~
(def l1 (construct List JavaArray))
(java-array-list-add-all l (build-list-native 3 (lambda (x) (+ x 1))))
(java-array-list-foldr / 0 l) ;; = 1.5
~~~
### <a name="velka.core.langbase.JavaArrayList$25"> everyp(java-array-list-everyp)</a>
Syntax:

~~~
(java-array-list-everyp <list> <predicate>)
~~~

Type Signature:

~~~
[List:JavaArray, [SYSGENNAMEcgo] -> Bool:Native] -> Bool:Native
~~~

Returns true if every element of this list returns true for the predicate. Otherwise returns false.

Example:

~~~
(define l (construct List JavaArray))
(java-array-list-add-all l (build-list-native 10 (* 2 x)))
(java-array-list-everyp l (lambda (x) (= (mod x 2) 0))) ;; = #t
(java-array-list-everyp l (lambda (x) (= x 1))) ;; = #f
~~~