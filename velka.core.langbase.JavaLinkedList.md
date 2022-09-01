# Linked List
Operators for working with wrapped java.util.LinkedList.

## Table of Contents
* [constructor(velka.core.langbase.JavaLinkedList$1@3d8314f0)](#velka.core.langbase.JavaLinkedList$1)
* [addToEnd(java-linked-list-add-to-end)](#velka.core.langbase.JavaLinkedList$2)
* [addToIndex(java-linked-list-add-to-index)](#velka.core.langbase.JavaLinkedList$3)
* [addAll(java-linked-list-add-all)](#velka.core.langbase.JavaLinkedList$4)
* [contains(java-linked-list-contains)](#velka.core.langbase.JavaLinkedList$5)
* [containsAll(java-linked-list-contains-all)](#velka.core.langbase.JavaLinkedList$6)
* [get(java-linked-list-get)](#velka.core.langbase.JavaLinkedList$7)
* [indexOf(java-linked-list-index-of)](#velka.core.langbase.JavaLinkedList$8)
* [isEmpty(java-linked-list-is-empty)](#velka.core.langbase.JavaLinkedList$9)
* [lastIndexOf(java-linked-list-last-index-of)](#velka.core.langbase.JavaLinkedList$10)
* [remove(java-linked-list-remove)](#velka.core.langbase.JavaLinkedList$11)
* [removeAll(java-linked-list-remove-all)](#velka.core.langbase.JavaLinkedList$12)
* [retainAll(java-linked-list-retain-all)](#velka.core.langbase.JavaLinkedList$13)
* [set(java-linked-list-set)](#velka.core.langbase.JavaLinkedList$14)
* [size(java-linked-list-size)](#velka.core.langbase.JavaLinkedList$15)
* [sublist(java-linked-list-sublist)](#velka.core.langbase.JavaLinkedList$16)
* [map(java-linked-list-map2)](#velka.core.langbase.JavaLinkedList$17)
* [map2(java-linked-list-map2)](#velka.core.langbase.JavaLinkedList$18)
* [foldl(java-linked-list-foldl)](#velka.core.langbase.JavaLinkedList$19)
* [foldr(java-linked-list-foldr)](#velka.core.langbase.JavaLinkedList$20)
* [LinkedListToArrayList(linked-list-2-array-list)](#velka.core.langbase.JavaLinkedList$21)
* [LinkedListToNativeList(linked-list-2-native-list)](#velka.core.langbase.JavaLinkedList$22)
* [everyp(java-linked-list-everyp)](#velka.core.langbase.JavaLinkedList$23)

## Constructors
### <a name="velka.core.langbase.JavaLinkedList$1"> constructor(velka.core.langbase.JavaLinkedList$1@3d8314f0)</a>
Syntax:

~~~
(construct List JavaLinked)
~~~

Type Signature:

~~~
[] -> List:JavaLinked
~~~

Constructs empty List:Linked.

## Operators
### <a name="velka.core.langbase.JavaLinkedList$2"> addToEnd(java-linked-list-add-to-end)</a>
Syntax:

~~~
(java-linked-list-add-to-end <list> <element>)
~~~

Type Signature:

~~~
[List:JavaLinked, SYSGENNAMEbjk] -> Bool:Native
~~~

Appends the specified element to the end of this list.

Example:

~~~
(java-linked-list-add-to-end (construct List JavaLinked) 42)
~~~
### <a name="velka.core.langbase.JavaLinkedList$3"> addToIndex(java-linked-list-add-to-index)</a>
Syntax:

~~~
(java-linked-list-add-to-index <list> <index> <element>)
~~~

Type Signature:

~~~
[List:JavaLinked, Int:Native, SYSGENNAMEbkh] -> []
~~~

Inserts the specified element at the specified position in this list.

Example:

~~~
(java-linked-list-add-to-index (construct List JavaLinked) 0 42)
~~~
### <a name="velka.core.langbase.JavaLinkedList$4"> addAll(java-linked-list-add-all)</a>
Syntax:

~~~
(java-linked-list-add-all <list1> <list2>)
~~~

Type Signature:

~~~
[List:JavaLinked, List:JavaLinked] -> Bool:Native
~~~

Appends all of the elements in the specified collection to the end of this list, in the order that they are returned by the specified collection's Iterator.

Example:

~~~
(def l (construct List JavaLinked))
(java-linked-list-add l 42)
(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))
(println l)
;;(42 0 1 2)
~~~
### <a name="velka.core.langbase.JavaLinkedList$5"> contains(java-linked-list-contains)</a>
Syntax:

~~~
(java-linked-list-contains <list> <element>)
~~~

Type Signature:

~~~
[List:JavaLinked, SYSGENNAMEbma] -> Bool:Native
~~~

Returns true if this list contains the specified element.

Example:

~~~
(java-linked-list-contains (construct List JavaLinked) 42) ; = #f
~~~
### <a name="velka.core.langbase.JavaLinkedList$6"> containsAll(java-linked-list-contains-all)</a>
Syntax:

~~~
(java-linked-list-contains-all <list1> <list2>)
~~~

Type Signature:

~~~
[List:JavaLinked, List:JavaLinked] -> Bool:Native
~~~

Returns true if this list contains all of the elements in the specified list.

Example:

~~~
(def l (construct List JavaLinked))
(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))
(java-linked-list-contains-all k (build-list-native 2 (lambda (x) x))) ;; = #t
~~~
### <a name="velka.core.langbase.JavaLinkedList$7"> get(java-linked-list-get)</a>
Syntax:

~~~
(java-linked-list-get <list> <index>)
~~~

Type Signature:

~~~
[List:JavaLinked, Int:Native] -> SYSGENNAMEbnt
~~~

Returns the element at the specified position in this list.

Example:

~~~
(def l (construct List JavaLinked))
(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))
(java-linked-list-get l 1) ;; = 1
~~~
### <a name="velka.core.langbase.JavaLinkedList$8"> indexOf(java-linked-list-index-of)</a>
Syntax:

~~~
(java-linked-list-index-of <list> <element>)
~~~

Type Signature:

~~~
[List:JavaLinked, SYSGENNAMEboq] -> Int:Native
~~~

Returns the index of the first occurrence of the specified element in this list, or -1 if this list does not contain the element.

Example:

~~~
(def l (construct List JavaLinked))
(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))
(java-linked-list-index-of l 1) ;; = 1
~~~
### <a name="velka.core.langbase.JavaLinkedList$9"> isEmpty(java-linked-list-is-empty)</a>
Syntax:

~~~
(java-linked-list-is-empty <list>)
~~~

Type Signature:

~~~
[List:JavaLinked] -> Bool:Native
~~~

Returns true if this list contains no elements.

Example:

~~~
(java-linked-list-is-empty (construct List JavaLinked)) ;; = #t
~~~
### <a name="velka.core.langbase.JavaLinkedList$10"> lastIndexOf(java-linked-list-last-index-of)</a>
Syntax:

~~~
(java-linked-list-last-index-of <list> <element>)
~~~

Type Signature:

~~~
[List:JavaLinked, SYSGENNAMEbqj] -> Int:Native
~~~

Returns the index of the last occurrence of the specified element in this list, or -1 if this list does not contain the element.

Example:

~~~
(def l (construct List JavaLinked))
(java-linked-list-add-all l (build-list-native 3 (lambda (x) 1)))
(java-linked-list-last-index-of l 1) ;; = 2
~~~
### <a name="velka.core.langbase.JavaLinkedList$11"> remove(java-linked-list-remove)</a>
Syntax:

~~~
(java-linked-list-remove <list> <element>)
~~~

Type Signature:

~~~
[List:JavaLinked, SYSGENNAMEbrg] -> Bool:Native
~~~

Removes the first occurrence of the specified element from this list, if it is present.

Example:

~~~
(def l (construct List JavaLinked))
(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))
(java-linked-list-remove l 1)
(println l)
(0 2)
~~~
### <a name="velka.core.langbase.JavaLinkedList$12"> removeAll(java-linked-list-remove-all)</a>
Syntax:

~~~
(java-linked-list-remove <list> <element>)
~~~

Type Signature:

~~~
[List:JavaLinked, List:JavaLinked] -> Bool:Native
~~~

Removes from this list all of its elements that are contained in the specified collection.

Example:

~~~
(def l (construct List JavaLinked))
(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))
(java-linked-list-add-all l (build-list-native 3 (lambda (x) 1)))
(println l)
(0 1 2 1 1 1)
(java-linked-list-remove l 1)
(println l)
(0 2)
~~~
### <a name="velka.core.langbase.JavaLinkedList$13"> retainAll(java-linked-list-retain-all)</a>
Syntax:

~~~
(java-linked-list-retain-all <retained-list> <retainee-list>)
~~~

Type Signature:

~~~
[List:JavaLinked, List:JavaLinked] -> Bool:Native
~~~

Retains only the elements in this list that are contained in the specified collection.

Example:

~~~
(def l (construct List JavaLinked))
(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))
(java-linked-list-add-all l (build-list-native 3 (lambda (x) 1)))
(java-linked-list-retain-all l (build-list-native 2 (lambda (x) (+ 1 x))))
(println l)
(2 3)
~~~
### <a name="velka.core.langbase.JavaLinkedList$14"> set(java-linked-list-set)</a>
Syntax:

~~~
(java-linked-list-set <list> <index> <element>)
~~~

Type Signature:

~~~
[List:JavaLinked, Int:Native, SYSGENNAMEbtv] -> SYSGENNAMEbtv
~~~

Replaces the element at the specified position in this list with the specified element.

Example:

~~~
(def l (construct List JavaLinked))
(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))
(java-linked-list-set l 1 42)
(println l)
(0 42 2)
~~~
### <a name="velka.core.langbase.JavaLinkedList$15"> size(java-linked-list-size)</a>
Syntax:

~~~
(java-linked-list-size <list>)
~~~

Type Signature:

~~~
[List:JavaLinked] -> Int:Native
~~~

Returns the number of elements in this list.

Example:

~~~
(def l (construct List JavaLinked))
(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))
(java-linked-list-size l) ;; = 3
~~~
### <a name="velka.core.langbase.JavaLinkedList$16"> sublist(java-linked-list-sublist)</a>
Syntax:

~~~
(java-linked-list-sublist <list> <fromIndex> <toIndex>)
~~~

Type Signature:

~~~
[List:JavaLinked, Int:Native, Int:Native] -> List:JavaLinked
~~~

Returns a view of the portion of this list between the specified fromIndex, inclusive, and toIndex, exclusive.

Example:

~~~
(def l (construct List JavaLinked))
(java-linked-list-add-all l (build-list-native 10 (lambda (x) x)))
(java-linked-list-sublist l 3 7)
;; = (2 3 4 5 6 7)
~~~
### <a name="velka.core.langbase.JavaLinkedList$17"> map(java-linked-list-map2)</a>
Syntax:

~~~
(java-linked-list-map <list> <function>)
~~~

Type Signature:

~~~
[List:JavaLinked, [SYSGENNAMEbwk] -> SYSGENNAMEbwl] -> List:JavaLinked
~~~

Returns a List:JavaLinked consisting of the results of applying the given function to the elements of list.

Example:

~~~
(def l (construct List JavaLinked))
(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))
(java-linked-list-map l (lambda (x) (+ x 2)))
;; = (2 3 4)
~~~
### <a name="velka.core.langbase.JavaLinkedList$18"> map2(java-linked-list-map2)</a>
Syntax:

~~~
(java-linked-list-map2 <list1> <list2> <function>)
~~~

Type Signature:

~~~
[List:JavaLinked, List:JavaLinked, [SYSGENNAMEbxi, SYSGENNAMEbxj] -> SYSGENNAMEbxk] -> List:JavaLinked
~~~

Returns a List:JavaLinked consisting of the results of applying the given function to the elements of list1 and list2.

Example:

~~~
(def l1 (construct List JavaLinked))
(java-linked-list-add-all l1 (build-list-native 3 (lambda (x) x)))
(def l2 (construct List JavaLinked))
(java-linked-list-add-all l2 (build-list-native 3 (lambda (x) (+ x 1))))
(java-linked-list-map2 l1 l2 +)
;; = (1 3 5)
~~~
### <a name="velka.core.langbase.JavaLinkedList$19"> foldl(java-linked-list-foldl)</a>
Syntax:

~~~
(java-linked-list-foldl <function> <terminator> <list>)
~~~

Type Signature:

~~~
[[SYSGENNAMEbyh, SYSGENNAMEbyh] -> SYSGENNAMEbyh, SYSGENNAMEbyh, List:JavaLinked] -> SYSGENNAMEbyh
~~~

Performs a reduction on the elements of list, using the terminator value and an associative accumulation function, and returns the reduced value. Processes list from the beginning.

Example:

~~~
(def l1 (construct List JavaLinked))
(java-linked-list-add-all l (build-list-native 3 (lambda (x) (+ x 1))))
(java-linked-list-foldl / 0 l) ;; = 0.16666666666666666666666666666667
~~~
### <a name="velka.core.langbase.JavaLinkedList$20"> foldr(java-linked-list-foldr)</a>
Syntax:

~~~
(java-linked-list-foldr <function> <terminator> <list>)
~~~

Type Signature:

~~~
[[SYSGENNAMEbze, SYSGENNAMEbze] -> SYSGENNAMEbze, SYSGENNAMEbze, List:JavaLinked] -> SYSGENNAMEbze
~~~

Performs a reduction on the elements of list, using the terminator value and an associative accumulation function, and returns the reduced value. Processes list from the end.

Example:

~~~
(def l1 (construct List JavaLinked))
(java-linked-list-add-all l (build-list-native 3 (lambda (x) (+ x 1))))
(java-linked-list-foldr / 0 l) ;; = 1.5
~~~
### <a name="velka.core.langbase.JavaLinkedList$21"> LinkedListToArrayList(linked-list-2-array-list)</a>
Syntax:

~~~
(linked-list-2-array-list <linked-list>)
~~~

Type Signature:

~~~
[List:JavaLinked] -> List:JavaArray
~~~

Converts List:JavaLinked to List:JavaArray)

Example:

~~~
(linked-list-2-array-list (construct List JavaLinked))
~~~
### <a name="velka.core.langbase.JavaLinkedList$22"> LinkedListToNativeList(linked-list-2-native-list)</a>
Syntax:

~~~
(linked-list-2-native-list <linked list>)
~~~

Type Signature:

~~~
[List:JavaLinked] -> List:Native
~~~

Converts List:JavaLinked to List:Native.

Example:

~~~
(linked-list-2-native-list (construct List JavaLinked))
~~~
### <a name="velka.core.langbase.JavaLinkedList$23"> everyp(java-linked-list-everyp)</a>
Syntax:

~~~
(java-linked-list-everyp <list> <predicate>)
~~~

Type Signature:

~~~
[List:JavaLinked, [SYSGENNAMEcbt] -> Bool:Native] -> Bool:Native
~~~

Returns true if every element of this list returns true for the predicate. Otherwise returns false.

Example:

~~~
(define l (construct List JavaLinked))
(java-linked-list-add-all l (build-list-native 10 (* 2 x)))
(java-linked-list-everyp l (lambda (x) (= (mod x 2) 0))) ;; = #t
(java-linked-list-everyp l (lambda (x) (= x 1))) ;; = #f
~~~