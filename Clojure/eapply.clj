(def vectorDist (fn [v1 v2]
                    (reduce + (map (fn [x y] (if (= x y) 0 1)) v1 v2))))
     
(defmacro mytest [expr]
    (let  [rslt (eval expr)]
        (list println (list str (list str expr) " => " (list pr-str rslt)))))
                    

(mytest '(vectorDist [:IntRoman :NameStructured] [:IntNative :NameStructured])) ;1
(mytest '(vectorDist [:IntRoman :NameStructured] [:IntRoman :NameStructured])) ;0

(def rankImpls (fn [v impls]
    (map (fn [u] [(vectorDist (get u 0) v) (get u 1)]) impls))) 
    
(mytest '(rankImpls [:IntRoman :NameStructured] '([[:IntRoman :NameStructured] 0] [[:IntNative :NameStructured] 1] [[:IntNative :NameUnstructured] 2])));([0 0] [1 1] [2 2])

(def getImpl (fn [type elambda]
    (get (reduce (fn [x y] (if (< (get x 0) (get y 0)) x y)) (rankImpls type elambda)) 1)))
    
(mytest '(getImpl [:IntRoman :NameStructured] '([[:IntRoman :NameStructured] 0] [[:IntNative :NameStructured] 1] [[:IntNative :NameUnstructured] 2]))); 0
(mytest '(getImpl [:IntNative :NameStructured] '([[:IntRoman :NameStructured] 0] [[:IntNative :NameStructured] 1] [[:IntNative :NameUnstructured] 2]))); 1
(mytest '(getImpl [:IntNative :NameUnstructured] '([[:IntRoman :NameStructured] 0] [[:IntNative :NameStructured] 1] [[:IntNative :NameUnstructured] 2]))); 2

(def eapply-unfolded (fn [elambda type args]
    (let [impl (getImpl type elambda)]
        (apply impl args))))
    
(mytest '(eapply-unfolded `([[:IntNative] ~(fn [x] "Native")] [[:IntString] ~(fn [x] "String")] [[:IntRoman] ~(fn [x] "Roman")]) [:IntNative] [nil])) ;"Native"
(mytest '(eapply-unfolded `([[:IntNative] ~(fn [x] "Native")] [[:IntString] ~(fn [x] "String")] [[:IntRoman] ~(fn [x] "Roman")]) [:IntString] [nil])) ;"String"
(mytest '(eapply-unfolded `([[:IntNative] ~(fn [x] "Native")] [[:IntString] ~(fn [x] "String")] [[:IntRoman] ~(fn [x] "Roman")]) [:IntRoman] [nil])) ;"Roman"

(defn eapply-1 [elambda type args]
    (letfn [(vectorDist [v1 v2] (reduce + (map (fn [x y] (if (= x y) 0 1)) v1 v2)))
            (rankImpls [v impls] (map (fn [u] [(vectorDist (get u 0) v) (get u 1)]) impls))
            (getImpl [type elambda] (get (reduce (fn [x y] (if (< (get x 0) (get y 0)) x y)) (rankImpls type elambda)) 1))]
        (apply (getImpl type elambda) args)))
        
(mytest '(eapply-1 `([[:IntNative] ~(fn [x] "Native")] [[:IntString] ~(fn [x] "String")] [[:IntRoman] ~(fn [x] "Roman")]) [:IntNative] [nil])) ;"Native"
(mytest '(eapply-1 `([[:IntNative] ~(fn [x] "Native")] [[:IntString] ~(fn [x] "String")] [[:IntRoman] ~(fn [x] "Roman")]) [:IntString] [nil])) ;"String"
(mytest '(eapply-1 `([[:IntNative] ~(fn [x] "Native")] [[:IntString] ~(fn [x] "String")] [[:IntRoman] ~(fn [x] "Roman")]) [:IntRoman] [nil])) ;"Roman"

(defmacro convertArgs [fArgs rArgs args] 
    (map (fn [fArg rArg arg] `(~(symbol (str (name rArg) "2" (name fArg))) ~arg)) fArgs rArgs args))
    
(println (macroexpand '(convertArgs [:IntNative] [:IntString] [0])))
(println (macroexpand '(convertArgs [:IntNative :NameStructured] [:IntString :NameUnstructured] [0 nil])))

(def IntNative2IntString (fn [x] ["0"]))

(defmacro eapply [elambda fArgs rArgs args]     
    `(eapply-1 ~elambda ~rArgs ~(vec (map (fn [fArg rArg arg] `(~(symbol (str (name rArg) "2" (name fArg))) ~arg)) fArgs rArgs args))))

(println (macroexpand '(eapply `([[:IntString] ~(fn [x] "String")]) [:IntString] [:IntNative] [0])))
(mytest '(eapply `([[:IntString] ~(fn [x] "String")]) [:IntString] [:IntNative] [0]))
(mytest '(eapply `([[:IntNative] ~(fn [x] "Native")] [[:IntString] ~(fn [x] "String")] [[:IntRoman] ~(fn [x] "Roman")]) [:IntString] [:IntNative] [0]))
