# Bit Set
Operators for working with wrapped java.util.BitSet

## Table of Contents
* [constructor(construct Set BitSet)](#velka.core.langbase.JavaBitSet$1)
* [nBitsConstructor(construct Set BitSet)](#velka.core.langbase.JavaBitSet$2)
* [and(bit-set-and)](#velka.core.langbase.JavaBitSet$3)
* [andNot(bit-set-and-not)](#velka.core.langbase.JavaBitSet$4)
* [cardinality(bit-set-cardinality)](#velka.core.langbase.JavaBitSet$5)
* [clear(bit-set-clear)](#velka.core.langbase.JavaBitSet$6)
* [clearBitIndex(bit-set-clear-bit-index)](#velka.core.langbase.JavaBitSet$7)
* [clearInterval(bit-set-clear-interval)](#velka.core.langbase.JavaBitSet$8)
* [clone(bit-set-clone)](#velka.core.langbase.JavaBitSet$9)
* [equals(bit-set-equalp)](#velka.core.langbase.JavaBitSet$10)
* [flip(bit-set-flip)](#velka.core.langbase.JavaBitSet$11)
* [flipInterval(bit-set-flip-interval)](#velka.core.langbase.JavaBitSet$12)
* [get(bit-set-get)](#velka.core.langbase.JavaBitSet$13)
* [getInterval(bit-set-get-interval)](#velka.core.langbase.JavaBitSet$14)
* [intersects(bit-set-intersects)](#velka.core.langbase.JavaBitSet$15)
* [isEmpty(bit-set-is-empty)](#velka.core.langbase.JavaBitSet$16)
* [length(bit-set-length)](#velka.core.langbase.JavaBitSet$17)
* [nextClearBit(bit-set-next-clear-bit)](#velka.core.langbase.JavaBitSet$18)
* [nextSetBit(bit-set-next-set-bit)](#velka.core.langbase.JavaBitSet$19)
* [or(bit-set-or)](#velka.core.langbase.JavaBitSet$20)
* [previousClearBit(bit-set-previous-clear-bit)](#velka.core.langbase.JavaBitSet$21)
* [previousSetBit(bit-set-previous-set-bit)](#velka.core.langbase.JavaBitSet$22)
* [set(bit-set-set)](#velka.core.langbase.JavaBitSet$23)
* [setValue(bit-set-set-value)](#velka.core.langbase.JavaBitSet$24)
* [setInterval(bit-set-set-interval)](#velka.core.langbase.JavaBitSet$25)
* [setIntervalValue(bit-set-set-interval-value)](#velka.core.langbase.JavaBitSet$26)
* [size(bit-set-size)](#velka.core.langbase.JavaBitSet$27)
* [str(bit-set-str)](#velka.core.langbase.JavaBitSet$28)
* [xor(bit-set-xor)](#velka.core.langbase.JavaBitSet$29)

## Constructors
### <a name="velka.core.langbase.JavaBitSet$1"> constructor(construct Set BitSet)</a>
Syntax:

~~~
(construct Set BitSet)
~~~

Type Signature:

~~~
[] -> Set:BitSet
~~~

Constructs empty Set:BitSet.
### <a name="velka.core.langbase.JavaBitSet$2"> nBitsConstructor(construct Set BitSet)</a>
Syntax:

~~~
(construct Set BitSet <nbits>)
~~~

Type Signature:

~~~
[Int:Native] -> Set:BitSet
~~~

Creates a bit set whose initial size is large enough to explicitly represent bits with indices in the range 0 through nbits-1.

## Operators
### <a name="velka.core.langbase.JavaBitSet$3"> and(bit-set-and)</a>
Syntax:

~~~
(bit-set-and <set1> <set2>)
~~~

Type Signature:

~~~
[Set:BitSet, Set:BitSet] -> Set:BitSet
~~~

Performs a logical AND of this target bit set with the argument bit set.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set-interval s1 0 5)
(define s2 (construct Set BitSet))
(bit-set-set-interval s2 3 7)
(bit-set-and s1 s2)
~~~
### <a name="velka.core.langbase.JavaBitSet$4"> andNot(bit-set-and-not)</a>
Syntax:

~~~
(bit-set-and-not <set1> <set2>)
~~~

Type Signature:

~~~
[Set:BitSet, Set:BitSet] -> Set:BitSet
~~~

Clears all of the bits in this BitSet whose corresponding bit is set in the specified BitSet.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set-interval s1 0 5)
(define s2 (construct Set BitSet))
(bit-set-set-interval s2 3 7)
(bit-set-and-not s1 s2)
~~~
### <a name="velka.core.langbase.JavaBitSet$5"> cardinality(bit-set-cardinality)</a>
Syntax:

~~~
(bit-set-cardinality <set>)
~~~

Type Signature:

~~~
[Set:BitSet] -> Int:Native
~~~

Returns the number of bits set to true in this BitSet.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set-interval s1 0 5)
(bit-set-cardinality s1)
~~~
### <a name="velka.core.langbase.JavaBitSet$6"> clear(bit-set-clear)</a>
Syntax:

~~~
(bit-set-clear <set>)
~~~

Type Signature:

~~~
[Set:BitSet] -> Set:BitSet
~~~

Sets all of the bits in this BitSet to false.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set-interval s1 0 5)
(bit-set-clear s1)
~~~
### <a name="velka.core.langbase.JavaBitSet$7"> clearBitIndex(bit-set-clear-bit-index)</a>
Syntax:

~~~
(bit-set-clear-bit-index <set> <index>)
~~~

Type Signature:

~~~
[Set:BitSet, Int:Native] -> Set:BitSet
~~~

Sets the bit specified by the index to false.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set-interval s1 0 5)
(bit-set-clear-bit-index s1 3)
~~~
### <a name="velka.core.langbase.JavaBitSet$8"> clearInterval(bit-set-clear-interval)</a>
Syntax:

~~~
(bit-set-clear-interval <set> <fromIndex> <toIndex>)
~~~

Type Signature:

~~~
[Set:BitSet, Int:Native, Int:Native] -> Set:BitSet
~~~

Sets the bits from the specified fromIndex (inclusive) to the specified toIndex (exclusive) to false.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set-interval s1 0 5)
(bit-set-clear-interval s1 2 4)
~~~
### <a name="velka.core.langbase.JavaBitSet$9"> clone(bit-set-clone)</a>
Syntax:

~~~
(bit-set-clone <set>)
~~~

Type Signature:

~~~
[Set:BitSet] -> Set:BitSet
~~~

Cloning this BitSet produces a new BitSet that is equal to it.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set-interval s1 0 5)
(bit-set-clone s1)
~~~
### <a name="velka.core.langbase.JavaBitSet$10"> equals(bit-set-equalp)</a>
Syntax:

~~~
(bit-set-equalp <set1> <set2>)
~~~

Type Signature:

~~~
[Set:BitSet, Set:BitSet] -> Bool:Native
~~~

Compares this object against the specified object.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set-interval s1 0 5)
(define s2 (construct Set BitSet))
(bit-set-set-interval s2 3 7)
(bit-set-equalp s1 s2)
~~~
### <a name="velka.core.langbase.JavaBitSet$11"> flip(bit-set-flip)</a>
Syntax:

~~~
(bit-set-flip <set> <bitIndex>)
~~~

Type Signature:

~~~
[Set:BitSet, Int:Native] -> Set:BitSet
~~~

Sets the bit at the specified index to the complement of its current value.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-flip s1 3)
~~~
### <a name="velka.core.langbase.JavaBitSet$12"> flipInterval(bit-set-flip-interval)</a>
Syntax:

~~~
(bit-set-flip-interval <set> <fromIndex> <toIndex>)
~~~

Type Signature:

~~~
[Set:BitSet, Int:Native, Int:Native] -> Set:BitSet
~~~

Sets each bit from the specified fromIndex (inclusive) to the specified toIndex (exclusive) to the complement of its current value.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-flip-interval s1 3 5)
~~~
### <a name="velka.core.langbase.JavaBitSet$13"> get(bit-set-get)</a>
Syntax:

~~~
(bit-set-get <set> <index>)
~~~

Type Signature:

~~~
[Set:BitSet, Int:Native] -> Bool:Native
~~~

Returns the value of the bit with the specified index.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set-interval s1 0 5)
(bit-set-get s1 3)
~~~
### <a name="velka.core.langbase.JavaBitSet$14"> getInterval(bit-set-get-interval)</a>
Syntax:

~~~
(bit-set-get-interval <set> <fromIndex> <toIndex>)
~~~

Type Signature:

~~~
[Set:BitSet, Int:Native, Int:Native] -> Set:BitSet
~~~

Returns a new BitSet composed of bits from this BitSet from fromIndex (inclusive) to toIndex (exclusive).

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set-interval s1 0 5)
(bit-set-get-interval s1 2 4)
~~~
### <a name="velka.core.langbase.JavaBitSet$15"> intersects(bit-set-intersects)</a>
Syntax:

~~~
(bit-set-intersects <set1> <set2>)
~~~

Type Signature:

~~~
[Set:BitSet, Set:BitSet] -> Set:BitSet
~~~

Returns true if the specified BitSet has any bits set to true that are also set to true in this BitSet.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set-interval s1 0 5)
(define s2 (construct Set BitSet))
(bit-set-set-interval s2 3 7)
(bit-set-intersects s1 s2)
~~~
### <a name="velka.core.langbase.JavaBitSet$16"> isEmpty(bit-set-is-empty)</a>
Syntax:

~~~
(bit-set-is-empty <set>)
~~~

Type Signature:

~~~
[Set:BitSet] -> Bool:Native
~~~

Returns true if this BitSet contains no bits that are set to true.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-is-empty s1)
(bit-set-set-interval s1 0 5)
(bit-set-is-empty s1)
~~~
### <a name="velka.core.langbase.JavaBitSet$17"> length(bit-set-length)</a>
Syntax:

~~~
(bit-set-length <set>)
~~~

Type Signature:

~~~
[Set:BitSet] -> Int:Native
~~~

Returns the "logical size" of this BitSet: the index of the highest set bit in the BitSet plus one.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set-interval s1 0 5)
(bit-set-length s1);; = 5
~~~
### <a name="velka.core.langbase.JavaBitSet$18"> nextClearBit(bit-set-next-clear-bit)</a>
Syntax:

~~~
(bit-set-next-clear-bit <set> <fromIndex>)
~~~

Type Signature:

~~~
[Set:BitSet, Int:Native] -> Int:Native
~~~

Returns the index of the first bit that is set to false that occurs on or after the specified starting index.


### <a name="velka.core.langbase.JavaBitSet$19"> nextSetBit(bit-set-next-set-bit)</a>
Syntax:

~~~
(bit-set-next-set-bit <set> <fromIndex>)
~~~

Type Signature:

~~~
[Set:BitSet, Int:Native] -> Int:Native
~~~

Returns the index of the first bit that is set to true that occurs on or after the specified starting index.


### <a name="velka.core.langbase.JavaBitSet$20"> or(bit-set-or)</a>
Syntax:

~~~
(bit-set-or <set1> <set2>)
~~~

Type Signature:

~~~
[Set:BitSet, Set:BitSet] -> Set:BitSet
~~~

Performs a logical OR of this bit set with the bit set argument.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set-interval s1 0 5)
(define s2 (construct Set BitSet))
(bit-set-set-interval s2 3 7)
(bit-set-or s1 s2)
~~~
### <a name="velka.core.langbase.JavaBitSet$21"> previousClearBit(bit-set-previous-clear-bit)</a>
Syntax:

~~~
(bit-set-previous-clear-bit <set> <fromIndex>)
~~~

Type Signature:

~~~
[Set:BitSet, Int:Native] -> Int:Native
~~~

Returns the index of the nearest bit that is set to false that occurs on or before the specified starting index.


### <a name="velka.core.langbase.JavaBitSet$22"> previousSetBit(bit-set-previous-set-bit)</a>
Syntax:

~~~
(bit-set-previous-set-bit <set> <fromIndex>)
~~~

Type Signature:

~~~
[Set:BitSet, Int:Native] -> Int:Native
~~~

Returns the index of the nearest bit that is set to true that occurs on or before the specified starting index.


### <a name="velka.core.langbase.JavaBitSet$23"> set(bit-set-set)</a>
Syntax:

~~~
(bit-set-set <set> <bitIndex>)
~~~

Type Signature:

~~~
[Set:BitSet, Int:Native] -> Set:BitSet
~~~

Sets the bit at the specified index to true.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set s1 3)
~~~
### <a name="velka.core.langbase.JavaBitSet$24"> setValue(bit-set-set-value)</a>
Syntax:

~~~
(bit-set-set-value <set> <bitIndex> <value>)
~~~

Type Signature:

~~~
[Set:BitSet, Int:Native, Bool:Native] -> Set:BitSet
~~~

Sets the bit at the specified index to the specified value.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set-value s1 3 #t)
~~~
### <a name="velka.core.langbase.JavaBitSet$25"> setInterval(bit-set-set-interval)</a>
Syntax:

~~~
(bit-set-set-interval <set> <fromIndex> <toIndex>)
~~~

Type Signature:

~~~
[Set:BitSet, Int:Native, Int:Native] -> Set:BitSet
~~~

Sets the bits from the specified fromIndex (inclusive) to the specified toIndex (exclusive) to true.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set-interval s1 3 5)
~~~
### <a name="velka.core.langbase.JavaBitSet$26"> setIntervalValue(bit-set-set-interval-value)</a>
Syntax:

~~~
(bit-set-set-interval-value <set> <fromIndex> <toIndex> <value>)
~~~

Type Signature:

~~~
[Set:BitSet, Int:Native, Int:Native, Bool:Native] -> Set:BitSet
~~~

Sets the bits from the specified fromIndex (inclusive) to the specified toIndex (exclusive) to the specified value.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set-interval-value s1 3 5 #f)
~~~
### <a name="velka.core.langbase.JavaBitSet$27"> size(bit-set-size)</a>
Syntax:

~~~
(bit-set-size <set>)
~~~

Type Signature:

~~~
[Set:BitSet] -> Int:Native
~~~

Returns the number of bits of space actually in use by this BitSet to represent bit values.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set-interval s1 3 5)
(bit-set-size s1)
~~~
### <a name="velka.core.langbase.JavaBitSet$28"> str(bit-set-str)</a>
Syntax:

~~~
(bit-set-str <set>)
~~~

Type Signature:

~~~
[Set:BitSet] -> String:Native
~~~

Returns a string representation of this bit set.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set-interval s1 3 5)
(bit-set-str s1)
~~~
### <a name="velka.core.langbase.JavaBitSet$29"> xor(bit-set-xor)</a>
Syntax:

~~~
(bit-set-xor <set1> <set2>)
~~~

Type Signature:

~~~
[Set:BitSet, Set:BitSet] -> Set:BitSet
~~~

Performs a logical XOR of this bit set with the bit set argument.

Example:

~~~
(define s1 (construct Set BitSet))
(bit-set-set-interval s1 0 5)
(define s2 (construct Set BitSet))
(bit-set-set-interval s2 3 7)
(bit-set-xor s1 s2)
~~~