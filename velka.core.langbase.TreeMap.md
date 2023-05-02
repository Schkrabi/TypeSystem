# Tree Map
Operators for working with java.util.TreeMap.

## Table of Contents
* [constructor(construct Map Tree)](#velka.core.langbase.TreeMap$1)
* [ceilingEntry(map-tree-ceiling-entry)](#velka.core.langbase.TreeMap$2)
* [ceilingKey(map-tree-ceiling-key)](#velka.core.langbase.TreeMap$3)
* [containsKey(map-tree-contains-key)](#velka.core.langbase.TreeMap$4)
* [containsValue(map-tree-contains-value)](#velka.core.langbase.TreeMap$5)
* [firstEntry(map-tree-first-entry)](#velka.core.langbase.TreeMap$6)
* [firstKey(map-tree-first-key)](#velka.core.langbase.TreeMap$7)
* [floorEntry(map-tree-floor-entry)](#velka.core.langbase.TreeMap$8)
* [floorKey(map-tree-floor-key)](#velka.core.langbase.TreeMap$9)
* [get(map-tree-get)](#velka.core.langbase.TreeMap$10)
* [headMap(map-tree-head)](#velka.core.langbase.TreeMap$11)
* [headMapIncl(map-tree-head-incl)](#velka.core.langbase.TreeMap$12)
* [higherEntry(map-tree-higher-entry)](#velka.core.langbase.TreeMap$13)
* [higherKey(map-tree-higher-key)](#velka.core.langbase.TreeMap$14)
* [keys(map-tree-keys)](#velka.core.langbase.TreeMap$15)
* [lastEntry(map-tree-last-entry)](#velka.core.langbase.TreeMap$16)
* [lastKey(map-tree-last-key)](#velka.core.langbase.TreeMap$17)
* [lowerEntry(map-tree-lower-entry)](#velka.core.langbase.TreeMap$18)
* [lowerKey(map-tree-lower-key)](#velka.core.langbase.TreeMap$19)
* [pollFirstEntry(map-tree-poll-first-entry)](#velka.core.langbase.TreeMap$20)
* [pollLastEntry(map-tree-poll-last-entry)](#velka.core.langbase.TreeMap$21)
* [put(map-tree-put)](#velka.core.langbase.TreeMap$22)
* [putAll(map-tree-put-all)](#velka.core.langbase.TreeMap$23)
* [remove(map-tree-remove)](#velka.core.langbase.TreeMap$24)
* [size(map-tree-size)](#velka.core.langbase.TreeMap$25)
* [subMapIncl(map-tree-sub-map-inclusive)](#velka.core.langbase.TreeMap$26)
* [subMap(map-tree-sub-map)](#velka.core.langbase.TreeMap$27)
* [tailMap(map-tree-tail-map)](#velka.core.langbase.TreeMap$28)
* [tailMapIncl(map-tree-tail-map-incl)](#velka.core.langbase.TreeMap$29)
* [values(map-tree-values)](#velka.core.langbase.TreeMap$30)

## Constructors
### <a name="velka.core.langbase.TreeMap$1"> constructor(construct Map Tree)</a>
Syntax:

~~~
(construct Map Tree <comparator function>)
~~~

Type Signature:

~~~
[[SYSGENNAMEfip, SYSGENNAMEfip] -> Int:Native] -> Map:Tree
~~~

Constructs Map:Tree.

## Operators
### <a name="velka.core.langbase.TreeMap$2"> ceilingEntry(map-tree-ceiling-entry)</a>
Syntax:

~~~
(map-tree-ceiling-entry <map>)
~~~

Type Signature:

~~~
[Map:Tree, SYSGENNAMEfjo] -> [SYSGENNAMEfjo, SYSGENNAMEfjp]
~~~

Returns a key-value mapping associated with the least key greater than or equal to the given key, or throws error if no such mapping exists.

Example:

~~~
(map-tree-ceiling-entry (construct Map Tree (lambda (x y) -1)))
~~~
### <a name="velka.core.langbase.TreeMap$3"> ceilingKey(map-tree-ceiling-key)</a>
Syntax:

~~~
(map-tree-ceiling-entry <map>)
~~~

Type Signature:

~~~
[Map:Tree, SYSGENNAMEfko] -> SYSGENNAMEfko
~~~

Returns the least key greater than or equal to the given key, or null if there is no such key.

Example:

~~~
(map-tree-ceiling-key (construct Map Tree (lambda (x y) -1)))
~~~
### <a name="velka.core.langbase.TreeMap$4"> containsKey(map-tree-contains-key)</a>
Syntax:

~~~
(map-tree-contains-key <map> <key>)
~~~

Type Signature:

~~~
[Map:Tree, SYSGENNAMEfln] -> Bool:Native
~~~

Returns true if this map contains a mapping for the specified key.

Example:

~~~
(map-tree-contains-key (construct Map Tree (lambda (x y) -1)) 1 )
~~~
### <a name="velka.core.langbase.TreeMap$5"> containsValue(map-tree-contains-value)</a>
Syntax:

~~~
(map-tree-contains-value <map> <value>)
~~~

Type Signature:

~~~
[Map:Tree, SYSGENNAMEfmm] -> Bool:Native
~~~

Returns true if this map maps one or more keys to the specified value.

Example:

~~~
(map-tree-contains-value (construct Map Tree (lambda (x y) -1)) "foo")
~~~
### <a name="velka.core.langbase.TreeMap$6"> firstEntry(map-tree-first-entry)</a>
Syntax:

~~~
(map-tree-first-entry <map>)
~~~

Type Signature:

~~~
[Map:Tree] -> [SYSGENNAMEfnl, SYSGENNAMEfnm]
~~~

Returns a key-value mapping associated with the least key in this map, or throws error if the map is empty.

Example:

~~~
(map-tree-first-entry (construct Map Tree (lambda (x y) -1)))
~~~
### <a name="velka.core.langbase.TreeMap$7"> firstKey(map-tree-first-key)</a>
Syntax:

~~~
(map-tree-first-key <map>)
~~~

Type Signature:

~~~
[Map:Tree] -> SYSGENNAMEfol
~~~

Returns the first (lowest) key currently in this map. Throws error if no such key exists.

Example:

~~~
(map-tree-first-key (construct Map Tree (lambda (x y) -1)))
~~~
### <a name="velka.core.langbase.TreeMap$8"> floorEntry(map-tree-floor-entry)</a>
Syntax:

~~~
(map-tree-floor-entry <map> <key>)
~~~

Type Signature:

~~~
[Map:Tree, SYSGENNAMEfpk] -> [SYSGENNAMEfpk, SYSGENNAMEfpl]
~~~

Returns a key-value mapping associated with the greatest key less than or equal to the given key, or throws error if there is no such key.

Example:

~~~
(map-tree-floor-entry (construct Map Tree (lambda (x y) -1)) 1 )
~~~
### <a name="velka.core.langbase.TreeMap$9"> floorKey(map-tree-floor-key)</a>
Syntax:

~~~
(map-tree-floor-key <map> <key>)
~~~

Type Signature:

~~~
[Map:Tree, SYSGENNAMEfqk] -> SYSGENNAMEfqk
~~~

Returns the greatest key less than or equal to the given key, or throws error if there is no such key.

Example:

~~~
(map-tree-floor-key (construct Map Tree (lambda (x y) -1)) 1 )
~~~
### <a name="velka.core.langbase.TreeMap$10"> get(map-tree-get)</a>
Syntax:

~~~
(map-tree-get <map> <key>)
~~~

Type Signature:

~~~
[Map:Tree, SYSGENNAMEfrj] -> SYSGENNAMEfrk
~~~

Returns the value to which the specified key is mapped, or null if this map contains no mapping for the key.

Example:

~~~
(map-tree-get (construct Map Tree (lambda (x y) -1)) 1 )
~~~
### <a name="velka.core.langbase.TreeMap$11"> headMap(map-tree-head)</a>
Syntax:

~~~
(map-tree-head <map> <to-key> <value>)
~~~

Type Signature:

~~~
[Map:Tree, SYSGENNAMEfsj] -> Map:Tree
~~~

Returns a view of the portion of this map whose keys are less than (or equal to, if inclusive is true) to-key.

Example:

~~~
(map-tree-head (construct Map Tree (lambda (x y) -1)) 1 "foo")
~~~
### <a name="velka.core.langbase.TreeMap$12"> headMapIncl(map-tree-head-incl)</a>
Syntax:

~~~
(map-tree-head-incl <map> <to-key> <inclusive?>)
~~~

Type Signature:

~~~
[Map:Tree, SYSGENNAMEfti, Bool:Native] -> Map:Tree
~~~

Returns a view of the portion of this map whose keys are less than (or equal to, if inclusive? is true) to-key.

Example:

~~~
(map-tree-head-incl (construct Map Tree (lambda (x y) -1)) 1 #f)
~~~
### <a name="velka.core.langbase.TreeMap$13"> higherEntry(map-tree-higher-entry)</a>
Syntax:

~~~
(map-tree-higher-entry <map> <key>)
~~~

Type Signature:

~~~
[Map:Tree, SYSGENNAMEfuh] -> [SYSGENNAMEfuh, SYSGENNAMEfui]
~~~

Returns a key-value mapping associated with the least key strictly greater than the given key, or null if there is no such key.

Example:

~~~
(map-tree-higher-entry (construct Map Tree (lambda (x y) -1)) 1)
~~~
### <a name="velka.core.langbase.TreeMap$14"> higherKey(map-tree-higher-key)</a>
Syntax:

~~~
(map-tree-higher-key <map> <key>)
~~~

Type Signature:

~~~
[Map:Tree, SYSGENNAMEfvh] -> SYSGENNAMEfvh
~~~

Returns the least key strictly greater than the given key, or throws error if there is no such key.

Example:

~~~
(map-tree-higher-key (construct Map Tree (lambda (x y) -1)) 1)
~~~
### <a name="velka.core.langbase.TreeMap$15"> keys(map-tree-keys)</a>
Syntax:

~~~
(map-tree-keys <map>)
~~~

Type Signature:

~~~
[Map:Tree] -> List:Native
~~~

Returns a Set view of the keys contained in this map.

Example:

~~~
(map-tree-keys (construct Map Tree (lambda (x y) -1)))
~~~
### <a name="velka.core.langbase.TreeMap$16"> lastEntry(map-tree-last-entry)</a>
Syntax:

~~~
(map-tree-last-entry <map>)
~~~

Type Signature:

~~~
[Map:Tree] -> [SYSGENNAMEfxe, SYSGENNAMEfxf]
~~~

Returns a key-value mapping associated with the greatest key in this map, or throws error if the map is empty.

Example:

~~~
(map-tree-last-entry (construct Map Tree (lambda (x y) -1)))
~~~
### <a name="velka.core.langbase.TreeMap$17"> lastKey(map-tree-last-key)</a>
Syntax:

~~~
(map-tree-last-key <map>)
~~~

Type Signature:

~~~
[Map:Tree] -> SYSGENNAMEfye
~~~

Returns the last (highest) key currently in this map.

Example:

~~~
(map-tree-last-key (construct Map Tree (lambda (x y) -1)))
~~~
### <a name="velka.core.langbase.TreeMap$18"> lowerEntry(map-tree-lower-entry)</a>
Syntax:

~~~
(map-tree-lower-entry <map> <key>)
~~~

Type Signature:

~~~
[Map:Tree, SYSGENNAMEfzd] -> [SYSGENNAMEfzd, SYSGENNAMEfze]
~~~

Returns a key-value mapping associated with the greatest key strictly less than the given key, or throws error if there is no such key.

Example:

~~~
(map-tree-lower-entry (construct Map Tree (lambda (x y) -1)) 1)
~~~
### <a name="velka.core.langbase.TreeMap$19"> lowerKey(map-tree-lower-key)</a>
Syntax:

~~~
(map-tree-lower-key <map> <key>)
~~~

Type Signature:

~~~
[Map:Tree, SYSGENNAMEgad] -> SYSGENNAMEgad
~~~

Returns the greatest key strictly less than the given key, or throws an error if there is no such key.

Example:

~~~
(map-tree-lower-key (construct Map Tree (lambda (x y) -1)) 1)
~~~
### <a name="velka.core.langbase.TreeMap$20"> pollFirstEntry(map-tree-poll-first-entry)</a>
Syntax:

~~~
(map-tree-poll-first-entry <map>)
~~~

Type Signature:

~~~
[Map:Tree] -> [SYSGENNAMEgbc, SYSGENNAMEgbd]
~~~

Removes and returns a key-value mapping associated with the least key in this map, or throws error if the map is empty.

Example:

~~~
(map-tree-poll-first-entry (construct Map Tree (lambda (x y) -1)))
~~~
### <a name="velka.core.langbase.TreeMap$21"> pollLastEntry(map-tree-poll-last-entry)</a>
Syntax:

~~~
(map-tree-poll-last-entry <map>)
~~~

Type Signature:

~~~
[Map:Tree] -> [SYSGENNAMEgcc, SYSGENNAMEgcd]
~~~

Removes and returns a key-value mapping associated with the greatest key in this map, or throws error if the map is empty.

Example:

~~~
(map-tree-poll-last-entry (construct Map Tree (lambda (x y) -1)))
~~~
### <a name="velka.core.langbase.TreeMap$22"> put(map-tree-put)</a>
Syntax:

~~~
(map-tree-put <map> <key> <value>)
~~~

Type Signature:

~~~
[Map:Tree, SYSGENNAMEgdc, SYSGENNAMEgdd] -> Map:Tree
~~~

Associates the specified value with the specified key in the map.

Example:

~~~
(map-tree-put (construct Map Tree (lambda (x y) -1)) 1 "foo")
~~~
### <a name="velka.core.langbase.TreeMap$23"> putAll(map-tree-put-all)</a>
Syntax:

~~~
(map-tree-put-all <receiving-map> <source-map>)
~~~

Type Signature:

~~~
[Map:Tree, Map:Tree] -> Map:Tree
~~~

Copies all of the mappings from the specified map to this map.

Example:

~~~
(map-tree-put-all (construct Map Tree (lambda (x y) -1)) (map-tree-put (construct Map Tree (lambda (x y) -1) 1 "foo")))
~~~
### <a name="velka.core.langbase.TreeMap$24"> remove(map-tree-remove)</a>
Syntax:

~~~
(map-tree-remove <map> <key>)
~~~

Type Signature:

~~~
[Map:Tree, SYSGENNAMEgfa] -> SYSGENNAMEgfb
~~~

Removes the mapping for this key from this TreeMap if present.

Example:

~~~
(map-tree-remove (construct Map Tree (lambda (x y) -1)) 1)
~~~
### <a name="velka.core.langbase.TreeMap$25"> size(map-tree-size)</a>
Syntax:

~~~
(map-tree-size <map>)
~~~

Type Signature:

~~~
[Map:Tree] -> Int:Native
~~~

Returns the number of key-value mappings in this map.

Example:

~~~
(map-tree-size (construct Map Tree (lambda (x y) -1)))
~~~
### <a name="velka.core.langbase.TreeMap$26"> subMapIncl(map-tree-sub-map-inclusive)</a>
Syntax:

~~~
(map-tree-sub-map-inclusive <map> <from-key> <from-inclusive> <to-key> <to-inclusive>)
~~~

Type Signature:

~~~
[Map:Tree, SYSGENNAMEggy, Bool:Native, SYSGENNAMEggy, Bool:Native] -> Map:Tree
~~~

Returns a view of the portion of this map whose keys range from fromKey to toKey.

Example:

~~~
(map-tree-sub-map-inclusive (construct Map Tree (lambda (x y) -1)) 1 #t 3 #f)
~~~
### <a name="velka.core.langbase.TreeMap$27"> subMap(map-tree-sub-map)</a>
Syntax:

~~~
(map-tree-sub-map <map> <from-key> <to-key>)
~~~

Type Signature:

~~~
[Map:Tree, SYSGENNAMEghx, SYSGENNAMEghx] -> Map:Tree
~~~

Returns a view of the portion of this map whose keys range from fromKey, inclusive, to toKey, exclusive.

Example:

~~~
(map-tree-sub-map (construct Map Tree (lambda (x y) -1)) 1 3)
~~~
### <a name="velka.core.langbase.TreeMap$28"> tailMap(map-tree-tail-map)</a>
Syntax:

~~~
(map-tree-tail-map <map> <from-key>)
~~~

Type Signature:

~~~
[Map:Tree, SYSGENNAMEgiw] -> Map:Tree
~~~

Returns a view of the portion of this map whose keys are greater than or equal to from-key.

Example:

~~~
(map-tree-tail-map (construct Map Tree (lambda (x y) -1)) 1)
~~~
### <a name="velka.core.langbase.TreeMap$29"> tailMapIncl(map-tree-tail-map-incl)</a>
Syntax:

~~~
(map-tree-tail-map-incl <map> <from-key> <inclusive?>)
~~~

Type Signature:

~~~
[Map:Tree, SYSGENNAMEgjv, Bool:Native] -> Map:Tree
~~~

Returns a view of the portion of this map whose keys are greater than (or equal to, if inclusive is true) fromKey.

Example:

~~~
(map-tree-tail-map-incl (construct Map Tree (lambda (x y) -1)) 1 #t)
~~~
### <a name="velka.core.langbase.TreeMap$30"> values(map-tree-values)</a>
Syntax:

~~~
(map-tree-values <map>)
~~~

Type Signature:

~~~
[Map:Tree] -> List:Native
~~~

Returns a Collection view of the values contained in this map.

Example:

~~~
(map-tree-values (construct Map Tree (lambda (x y) -1)))
~~~