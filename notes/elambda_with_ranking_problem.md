# Problém ranking funkce obsažené v extended lambda výrazu

* Zdá se, že jestliže budeme zahrnovat ranking-function přímo v elambdách, ztratíme tím jemnou přesnost inferenčního algoritmu co se týče reprezentací jednotlivých typů.
* Argument uvedu v následujících sekcích.

## Inference a přesnost inferovaných reprezentací
* U inferenčního algoritmu by mělo jít dokázat, že typy a reprezentace, které vrací jsou přesné
* Předpoklad: Během inference by nemělo docházet k interpretaci podvýrazů s výjimkou ranking-funkcí
* Důkaz by probíhal strukturální indukcí, přes strukturu výrazu.
* Počáteční krok  by byl dokázat přesnost inference pro atomické výrazy
    * TODO rozpracovat důkaz
    * Atomické Literály
* Indukční krok by byl dokázat přesnost inference pro složené výrazy
    * TODO rozpracovat
    * Problém nastává v inferenci Aplikace Abstrakce
        * Aplikace abstrakce se skládá z výrazu na prvním místě a argumentů
        * výraz na prvním místě může být libovolný výraz jehož interpretací je abstrakce
        * Argumenty jsou n-tice (tuple) výrazů
    * Předpokládejme, že výsledkem interpretace výrazu na prvním místě je abstrakce, která má definovanou vlastní ranking funkci
    * Nicméně aby interpretační algoritmus získal abstrakci a tedy její ranking funkci, musel by výraz na prvním místě vyhodnotit. (slabý argument? tohle se dotýká vyčíslitelnosti a funkcí jako hodnoty prvního řádu)
    * Tedy inferenční algoritmus nemůže znát rankovací funkci a tím pádem ani přesně určit reprezentace typů.

## Proč to vadí při kompilaci?
* Předpokládejme opět, že při kompilaci nechceme provádět jinou interpretaci než interpretaci rankovacích funkcí.
* Tedy opět, kompilujeme-li apliakci abstrakce a výsledkem interpretace výrazu na prvním místě je abstrakce s vlastní rankovací funkcí:
    * tuto rankovací funkci nemůžeme získat aniž bychom výraz na prvním místě interpretovali
    * Tedy nemůžeme získat rankovací funkci a tedy v době kompilace ani přesné reprezentace typů jednotlivých výrazů
    * Tedy v době kompilace nemůžeme instancovat potřebné converze reprezentací
* Tento problém je řešitelný za cenu větší režije v Clojure:
    * Nechť se vzdáme inference přesné na úrovni reprezentací.
    * Při výpočtu v clojure jednotlivé clj výrazy nesou přesnou typovou a reprezentační informaci v metadatech
    * Tedy v clojure bude existovat funkce, která bude schopna instancovat na základě metadat konverze reprezentací dynamicky, během clojure runtime.
    
## Proč jsem se mýlil?
* Špatný byl základní předpoklad - nedává smysl vyhodnocovat rankovací funkci v době kompilace
  * V tu chvíli by fungovaly jenom velmi základní rankovací funkce, které by měly k dispozici pouze velmi základní definice a funkcionalitu
  * Navíc by to vedlo k nekonzistentnímu chování, kdy by uživatel mohl nadefinovat rankovací funkci někde v kódu a její applikace by proběhla normálně, ale když by ji použil jako rankovací funkci, pak by nastala chyba
  
## Jak změnit implementaci

Implementace se bude měnit v následujících výrazech:

### extended-lambda
* elambda nyní bude jediný výraz (jak při interpretaci tak v clojure) odpovědný za vybrání konkrétní implementace
* třída bude implementovat metodu 'Lambda getImplementation(Abstraction rankingFn)' nejspíše bude tato třída implementována na úrovni Abstrakce
* V clojure je extended-lambda funkce se dvěma argumenty - rankovací funcí a argumenty
* tato funkce bude vracet implementaci (tedy funkci) která bude nejvíce odpovídat rankovací funci a typu argumentů
* přesouvá se sem původní funkcionalita z eapply
* implementace bude vypadat zhruba takto:

~~~
(defn select-impl [ranking-fn args impls]
    (let [args-type (tuple2velka-list (:lang-type (meta args)))]
        (reduce min
            (map (fn [impl] [(apply ranking-fn 
                                [(tuple2velka-list (:l-type (:lang-type (meta impl))))
                                args-type])])
            impls))))

;;Elambda
(let [impls {impl1 impl2...}]
    (fn ([args] (select-impl elambda-ranking args impls))
        ([args ranking-fn] (select-impl ranking-fn args impls))))    
~~~

- výsledkem aplikace elambdy (v clojure) je implementace

### Lambda
* Lambda následuje v implementaci elambdu.
* V 'Lambda getImplementation(Abstraction rankingFn)' bude vracet samu sebe.
* V clojure bude lambda funkce ignorující své argumenty a vrací svou jedinou implementaci.

~~~
;;Lambda
(fn ([args] impl)
    ([args ranking-fn] impl))
~~~

### Aplikace
* nejdříve aplikuje elambdu na argumenty (a případně ranking function) čímž získá implementaci.
* z implementace získá očekávané typy argumentů
* vytvoří a vyhodnotí konverze
* tedy eapply bude vypadat následovně:

~~~
(fn eapply 
    ([abstraction args ranking]
        (let [  impl (abstraction args ranking)
                converted-args (convert args impl)]
            (apply impl converted-args)))
    ([abstraction args]
        (let [  impl (abstraction args)
                converted-args (convert args impl)]
            (apply impl converted-args))))  
~~~

### Convert
* Konverze jsou samy o sobě lambdy (ale ne elambdy)
* jestliže je 'C' hledaná konverze, pak tedy pro konverzi 'args' stačí:

~~~
((C args) args)
~~~

#### Konverze atomických typů
* atomické konverze jsou dvou druhů: vestavěné a definované uživatelem
* v clojure budou všechny atomické konverze uchovávány v mapě definované na počátku zdrojového kódu
* mapa bude mít tvar

~~~
{[fromType1 toType1] conv1 [fromType2 toType2] conv2...}
~~~

* tuto počáteční mapu bude vytvářet třída 'TypeSystem' a její jméno bude randomizované (pomocí NameGeneratoru)
* zároveň bude existovat funkce 'get-atom-conv from to', kterou bude generovat TypeSystem a bude definována hned za mapou:

~~~
(fn get-atom-conv [from to]
    (get type-map [from to]))
~~~

* Speciální forma 'define-conversion' se bude muset změnit a její clj implementace bude vypadat následovně:

~~~
(assoc type-map [from to] impl)
~~~

#### Konverze složených typů
* složené typy se budou convertovat pomocí specifických funkcí pro jednotlivé typy
* u type-arrow je arg lambda a proto tvar '(arg nil)' vrátí implmentaci
* konverze do representationOr nikdy nenastane
* 
* konverze z reprezentation or může nastat pouze v případě, že konvertovaný výraz je elambda
  * konverze nastává pouze v případě eapply
  * v eapply už jsou všechny argumenty vyhodnocené
  * vyhodnocením žádného výrazu s vyjímkou elambdy nemůže vzniknout výraz s representation or
* při předání elambdy na jejím typu záleží jenom při její aplikaci (proč?)
    * Ale při její aplikaci je stejně vyhodnocena rankovací funkce a vybrána implementace
    * Tedy nemá smysl konvertovat typ elambdy

~~~
(fn convert-tuple [from to arg] (map convert from to arg))
(fn convert-fn [from to arg] 
    (let [impl (fn [a] (convert 
                        (.rtype from) 
                        (.rtype to) 
                        (apply (arg nil) (convert (.ltype from) (.ltype to) a))))]
        (fn ([args] impl)
            ([args ranking-fn] impl))))
(fn convert-atom [from to arg]
    (((get-atom-conv from to) nil) arg))
~~~

TODO:
1. Přesunout odpovědnost za výběr implementace plně do elambdy
2. Udělat z lambda a fce jen speciální případ elambdy a efce
3. Přepsat překlad elambdy do clojure
4. Napsat converze v clojure
5. Přepsat eapply do clojure
