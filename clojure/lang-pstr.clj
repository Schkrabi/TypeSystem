;; (defrecord lang-type-atom [name representation])
;; (defrecord lang-type-arrow [arg-type return-type])

;; (def test-int (with-meta [42] {:lang-type (lang-type-atom. "Int" "Native")}))
;; (def test-string (with-meta ["foo"] {:lang-type (lang-type-atom. "String" "Native")}))
;; (def test-bool-t (with-meta [true] {:lang-type (lang-type-atom. "Bool" "Native")}))
;; (def test-bool-f (with-meta [false] {:lang-type (lang-type-atom. "Bool" "Native")}))
;; (def test-tuple-int (with-meta [(with-meta [42] {:lang-type (lang-type-atom. "Int" "Native")}) (with-meta [42] {:lang-type (lang-type-atom. "Int" "Native")})] {:lang-type [(lang-type-atom. "Int" "Native") (lang-type-atom. "Int" "Native")]}))
;; (def test-tuple-str (with-meta [(with-meta ["foo"] {:lang-type (lang-type-atom. "String" "Native")}) (with-meta ["foo"] {:lang-type (lang-type-atom. "String" "Native")})] {:lang-type [(lang-type-atom. "String" "Native") (lang-type-atom. "String    " "Native")]}))
;; (def test-fun (with-meta [(with-meta (fn [x] x) {:lang-type (lang-type-arrow. ["SYSGENNAMEv"] "SYSGENNAMEv")})] {:lang-type (lang-type-arrow. ["SYSGENNAMEv"] "SYSGENNAMEv")}))
;; (def test-composite (with-meta [(with-meta ["XLII"] {:lang-type (lang-type-atom. "String" "Native")})] {:lang-type (lang-type-atom. "Int" "Roman")}))

(def lang-pstr
    (fn [exp]
        (letfn [(lang-pstr-aux [exp level]
                    (let [type (:lang-type (meta exp))]
                        (cond
                            (or 
                                (= type velka.lang.types.TypeAtom/TypeIntNative)
                                (= type velka.lang.types.TypeAtom./TypeStringNative) 
                                (= type velka.lang.types.TypeAtom/TypeDoubleNative) 
                                (= type velka.lang.types.TypeAtom/TypeBoolNative)) 
                                    (if (= level 0) 
                                        (pr-str (get exp 0)) 
                                        (get exp 0))
                            (= type velka.lang.types.TypeTuple/EMPTY_TUPLE) []
                            (instance? velka.lang.types.TypeAtom type) (lang-pstr-aux (get exp 0) level)
                            (instance? velka.lang.types.TypeTuple type) 
                            (if 
                                (= level 0) 
                                (pr-str (vec (map (fn [x] (lang-pstr-aux x (+ level 1))) exp)))
                                (vec (map (fn [x] (lang-pstr-aux x (+ level 1))) exp)))
                            :else (throw (Throwable. (str exp " is not a printable expression"))))))]
            (lang-pstr-aux exp 0))))
