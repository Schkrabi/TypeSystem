(def roman2int (fn [arg]
                    (letfn [(romanCheck [arg] (re-matches #"(^(?=[MDCLXVI])M*(C[MD]|D?C{0,3})(X[CL]|L?X{0,3})(I[XV]|V?I{0,3})$)" arg))]
                        (when-not (romanCheck arg) (throw (Exception. (str "Invalid roman number " arg))))
                        (let [  values {\I 1
                                        \V 5
                                        \X 10
                                        \L 50
                                        \C 100
                                        \D 500
                                        \M 1000}
                                numbered (map (fn [x] (get values x)) arg)
                                first   (reverse (cons 0 numbered))
                                second  (cons 0 (reverse numbered))]
                            (reduce +(map (fn [cur prev]
                                        (if (< cur prev)
                                                (- cur)
                                                cur)) first second))))))
                                            
(def int2roman (fn [n]
                (let [  hundreds    ["" "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
                        tens        ["" "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
                        ones        ["" "I" "II" "III" "IV" "V" "VU" "VII" "VIII" "IX"]]
                        (letfn [(rec [n] 
                                    (if (>= n 1000)
                                        (str "M" (rec (- n 1000)))
                                        (str (get hundreds (quot n 100)) (get tens (quot (mod n 100) 10)) (get ones  (mod n 10)))))]
                            (rec n)))))

(def romanCheck (fn [str] (re-matches #"(^(?=[MDCLXVI])M*(C[MD]|D?C{0,3})(X[CL]|L?X{0,3})(I[XV]|V?I{0,3})$)" str)))
                                            
(println (roman2int "I"))
(println (roman2int "V"))
(println (roman2int "X"))
(println (roman2int "L"))
(println (roman2int "C"))
(println (roman2int "D"))
(println (roman2int "M"))

(println (roman2int "II"))
(println (roman2int "IV"))
(println (roman2int "MCMLXXXIX"))
(println (roman2int "Not Roman number"))

(println (int2roman 1))
(println (int2roman 5))
(println (int2roman 10))
(println (int2roman 50))
(println (int2roman 100))
(println (int2roman 1000))

(println (int2roman 2))
(println (int2roman 4))
(println (int2roman 1989))

