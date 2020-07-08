# Poznámky, převod výrazů jazyka do Clojure

## Reprezentace Typů

### Type Atom
* typový atom (tedy typ ve tvaru <TYP\>:<REPREZENTACE\>), reprezentujeme v clojure jako record s následující definicí

~~~
(defrecord lang-type-atom [name representation])
~~~

* jména typu i reprezentace jsou řetezce
* hodnoty se vytvářejí pomocí konstruktoru lang-type-atom.
* sloty se čtou pomocí (:slot hodnota) notace

Příklad:

~~~
Int:Native -CLJ-> (lang-type-atom. "Int" "Native")
~~~

### Type Tuple
* tuply se reprezentují jako clojurovský persistentVector svých podtypů

Příklad:

~~~
(Int:Native Name:Structured)
 |
CLJ
 |
 V
[(lang-type-atom. "Int" "Native") (lang-type-atom. "Name" "Structured")]
~~~

### Type Arrow
* funkční typy se reprezentují jako record s následující definicí

~~~
(defrecord lang-type-arrow [arg-type return-type])
~~~
    
* k vnitřním typům se přistujuje přes :key notaci
* v důsledku implementace lambda výrazů bude typ argumentu vždy typový tuple

Příklad:

~~~
(Int:Native Int:Native) #> Int:Native
 |
CLJ
 |
 V
(lang-type-arrow. 
    [   (lang-type-atom. "Int" "Native) 
        (lang-type-atom. "Int" "Native)] 
    (lang-type-atom. "Int" "Native))
~~~

### RepresentationOr
* representationOr (tedy množina typů lišících se pouze reprezentacemi svých typových atomů) se reprezentuje jako clojure set vnitřních jednotlivých typů
* representationOr nemá v jazyku syntaxi, zápis v jazyku níže je pouze ilustrační
* vzhledem k povaze representation or bude v clojure reprezentaci docházet k redundanci

Příklad:

~~~
{Int:Native Int:String}
 |
CLJ
 |
 V
#{(lang-type-atom "Int" "Native") (lang-type-atom "Int" "String")}
~~~

### Type Variable
* typová proměnná se reprezentuje jako string svého jména

~~~
A -CLJ-> "A"
~~~

## Překlad Výrazů do Clojure
* informace o typech v jednotlivých hodnotách se budou uchovávat v metadatech pod klíčem :lang-type ve formátu popsaném výše

### LitBoolean, LitInteger, LitString a LitDouble
* primitivní typy v clojure nepodporují metadata
* každý primitivní literál se bude obalovat persistentVectorem, který umožní uchovávat jednotným způsobem metadata o typu

Příklad:

~~~
42 -CLJ-> (with-meta [42] {:lang-type (lang-type-atom. "Int" "Native)})
"marvin" -CLJ-> (with-meta 
                    ["marvin"] 
                    {:lang-type (lang-type-atom. "String" "Native)})
~~~

### LitComposite
* stejně jako u primitivních typů se celá hodnota obalí persistentVectorem
* je to proto aby zůstaly zachovány informace o "vnitřním" typu hodnoty a daly se použít pro dekonstrukci

Příklad:

~~~
(construct Name Unstructured "Jan Novák")   
    -INTP-> <"Jan Novák" Name:Unstructured>
    -CLJ->  (with-meta 
                [(with-meta 
                    ["Jan Novák"]
                    {:lang-type (lang-type-atom. "String" "Native"}]
                {:lang-type (lang-type-atom. "Name" "Unstructured")})
~~~
                                            
### Tuple
* tuple se překládá jako persistentVector s metadaty o typu

Příklad:

~~~
(cons 42 "marvin")
 |
CLJ
 |
 V
(with-meta 
    [   (with-meta 
            [42] 
            {:lang-type (lang-type-atom. "Int" "Native")}) 
        (with-meta 
            ["marvin"] 
            {:lang-type (lang-type-atom. "String" "Native")})] 
    {:lang-type 
    [   (lang-type-atom. "Int" "Native") 
        (lang-type-atom. "String" "Native")]})
~~~
    
### Symbol
* protože se informace o typech používají v clojure až během runtime, nemusíme uchovávat metadata typů v proměnných (MŮŽE SE ZMĚNIT!)
* symbol se překládá do clojure jako své jméno

Příklad:

~~~
foo -CLJ-> foo
~~~
    
### Extended Lambda
* extended lambda výrazy překládáme do clojure jako persistentVecor jednotlivých implementací, které jsou reprezentovány clojure anonymními funkcemi ((fn...))
* persistentVector používáme, protože některé implementace se mohou lišit pouze v metadatech (viz. příklad)
* metainformace o typech jsou jednak na úrovni celé extended-lambdy (z důvodu dekonstrukce, reprezentované jako representationOr)
* druhak má každá implementace vlastní metadata kvůli volbě implementace při applikaci (viz. Abstraction-Application)

Příklad:

~~~
(extended-lambda ((Int x)) ((Int:Native) x) ((Int:String x)))
    |
   CLJ
    |
    V
(with-meta 
    [   (with-meta
            (fn [x] x)
            {:lang-type (lang-type-arrow. 
                            [(lang-type-atom. "Int" "Native")] 
                            (lang-type-atom. "Int" "Native"))})
        (with-meta
            (fn [x] x)
            {:lang-type (lang-type-arrow. 
                            [(lang-type-atom. "Int" "String")] 
                            (lang-type-atom. "Int" "String"))})]
    {:lang-type [   (lang-type-arrow. 
                        [(lang-type-atom. "Int" "Native")] 
                        (lang-type-atom. "Int" "Native"))
                    (lang-type-arrow. 
                            [(lang-type-atom. "Int" "String")] 
                            (lang-type-atom. "Int" "String"))])})
~~~
            
### Lambda
* všechny lambda výrazy se budou v clojure reprezentovat jako extended-lambda výrazy s jedinou implementací
* kvůli jednodušší implementaci eapply (viz. dále)
* kvůli tomu bude typ "obalové" `extended-lambdy` stejný jako typ její jedinné implementace (RepresentationOr zkolabuje do jednoho typu)

Příklad:

~~~
(lambda ((Int:Native x) (String:Native y)) x)
    |
   CLJ
    |
    V
(with-meta 
    [(with-meta 
        (fn [x y] x) 
        {:lang-type (lang-type-arrow. 
                        [   (lang-type-atom. "Int" "Native") 
                            (lang-type-atom. "String" "Native")]
                        (lang-type-atom. "Int" "Native"))})]
    {:lang-type (lang-type-arrow. 
                    [   (lang-type-atom. "Int" "Native") 
                        (lang-type-atom. "String" "Native")]
                    (lang-type-atom. "Int" "Native"))})
~~~
                        
### Operator
* operátory wrapované z Javy jsou zabalená do anonymní funkce
* anonymní funkce nese informace o typu
* anonymní funkce se statrá o extrakci clojure primitrivních typů z argumentů (viz. LitInteger)
* anonymní funkce musí rovněž znovu zabalit výsledek výpočtu do persistentVekctoru a označit ho příslušným typem
* pro operátory +, bit-and, bit-or, concat, equals?, <, *, not, =, - a println bude překlad vypadat podobně jako v následujícím příkladu:

Příklad:

~~~
+ -CLJ-> (with-meta 
            [(with-meta 
                (fn [x y] (with-meta 
                            [(+ (get x 0) (get y 0))]
                            {:lang-type (lang-type-atom. "Int" "Native")}))
                {:lang-type (lang-type-arrow.
                                [   (lang-type-atom. "Int" "Native") 
                                    (lang-type-atom. "Int" "Native")]
                                (lang-type-atom. "Int" "Native"))})]
            {:lang-type (lang-type-arrow.
                            [   (lang-type-atom. "Int" "Native") 
                                (lang-type-atom. "Int" "Native")]
                            (lang-type-atom. "Int" "Native"))})
~~~

#### Operátory car a cdr
* operátory car a cdr budou mít následující překlad do clojure

Příklad:

~~~
car -CLJ-> (with-meta 
            [(with-meta
                (fn [x] (get x 0))
                {:lang-type (lang-type-arrow. ["A" "B"] "A")})
            {:lang-type (lang-type-arrow. ["A" "B"] "A")})
car -CLJ-> (with-meta 
            [(with-meta
                (fn [x] (get x 1))
                {:lang-type (lang-type-arrow. ["A" "B"] "B")})
            {:lang-type (lang-type-arrow. ["A" "B"] "B")})
~~~

#### Konverzní operátory
* pro build-in typy jsou konverze definované jako operátory
* pro `IntNative2IntString`

~~~
(with-meta
    [(with-meta
        (fn [_x] (with-meta
                    [(with-meta
                        (Integer/toString (get _x 0))
                        {:lang-type (lang-type-atom. "String" "Native")})]
                    {:lang-type (lang-type-atom. "Int" "String")}))
        {:lang-type (lang-type-arrow. 
                        [(lang-type-atom. "Int" "Native")]
                        (lang-type-atom. "Int" "String"))})]
    {:lang-type (lang-type-arrow. 
                    [(lang-type-atom. "Int" "Native")]
                    (lang-type-atom. "Int" "String"))})
~~~

* pro `IntNative2IntRoman`
* int2RomanClojure je předpřipravená clojure anonymní funkce

~~~
(with-meta
    [(with-meta
        (fn [_x] (with-meta
                    [(with-meta
                        (int2RomanClojure (get _x 0))
                        {:lang-type (lang-type-atom. "String" "Native")})]
                    {:lang-type (lang-type-atom. "Int" "Roman")}))
        {:lang-type (lang-type-arrow. 
                        [(lang-type-atom. "Int" "Native")]
                        (lang-type-atom. "Int" "Roman"))})]
    {:lang-type (lang-type-arrow. 
                    [(lang-type-atom. "Int" "Native")]
                    (lang-type-atom. "Int" "Roman"))})
~~~

* pro `IntRoman2IntNative`
* `roman2intClojure` je předpřipravená clojure anonymní funkce

~~~
(with-meta 
	(fn [_x] (with-meta
				[(roman2intClojure (get (get _x 0) 0))]
				{:lang-type (lang-type-arom. "Int" "Native"})))
	{:lang-type (lang-type-arrow.
					[(lang-type-atom. "Int" "Roman")]
					(lang-type-atom. "Int" "Native"))});
~~~

* pro `IntRoman2IntString`
* nejdříve konvertujeme hodnotu na int a pak z ní uděláme string

~~~
(with-meta
	(fn [_x] (with-meta
				[(with-meta
					[(str (roman2intClojure (get (get _x 0) 0)))]
					{:lang-type (lang-type-atom. "String" "Native")})]
				{:lang-type (lang-type-atom. "Int" "String")}))
	{:lang-type (lang-type-arrow.
					[(lang-type-atom. "Int" "Roman")]
					(lang-type-atom. "Int" "String"))})
~~~

* pro `IntString2IntNative`

~~~
(with-meta
	(fn [_x] (with-meta
				[(Integer/parseInt (get (get _x 0) 0))]
				{:lang-type (lang-type-atom. "Int" "Native")}))
	{:lang-type (lang-type-arrow.
					[(lang-type-atom. "Int" "String")]
					(lang-type-atom. "Int" "Native"))})
~~~

* pro `IntString2IntRoman`

~~~
(with-meta
	(fn [_x] (with-meta
				[(with-meta
					[(int2romanClojure (Integer/parseInt (get (get _x 0) 0)))]
					{:lang-type (lang-type-atom. "String" "Native")}))]
				{:lang-type (lang-type-atom. "Int" "Roman")}))
	{:lang-type (lang-type-arrow.
					[(lang-type-atom. "Int" "String")]
					(lang-type-atom. "Int" "Roman"))})
~~~

#### Primitivní konstruktory
* primitivní konstruktory jsou v jazyku přítomny z důvodu úplnosti vzhlemdem ke speciální formě `construct`
* tedy aby bylo možné udělat následující (i když to nedává valný smysl):

~~~
(construct Int Native 42)
~~~

* primitivní konstruktory existují pro `Int:Native`, `Double:Native`, `Bool:Native` a `String:Native`
* všechny jsou implementovány identitou (clojure fce `identity`) následovně

~~~

(with-meta [
    (with-meta 
        (fn [_x] (identity _x))
        {:lang-type (lang-type-arrow. 
                        [(lang-type-atom. "Int" "Native")]
                        (lang-type-atom. "Int" "Native"))})
    {:lang-type (lang-type-arrow. 
                    [(lang-type-atom. "Int" "Native")]
                    (lang-type-atom. "Int" "Native"))})

~~~

### AbstractionApplication
* applikace abstrakcí (lambda-výrazů, extended-lambda výrazů, funkcí, extended-funkcí a operátorů) se provádý pomocí funkce `eapply`
* eapply má následující syntaxi

~~~
(eapply <abstraction> <arguments> <ranking-function>)
~~~

* <abstraction\> je do clojure přeložený lambda-výraz, extended-lambda nebo operátor (funkce a extended funkce jsou jenom vyhodnocené lambdy resp. extended-lambdy)
* <arguments\> je persistentVector obsahující reálné argumenty pro abstrakci
* <ranking-function\> je funkce ovlivňující výběr implementace pro extended-lambdy a extended-funkce
  * je to libobolná funkce přijímající jako první a druhý argument persistentVector typů (viz. výše) a vracející celé číslo
  * standardní implementace vypadá následovně

~~~
(fn [v1 v2] (reduce + (map (fn [x y] (if (= x y) 0 1)) v1 v2)))
~~~

  * za nejlepší implementaci se považuje ta s nejnižším výsledkem <ranking-function>
  * tato funkce je zatím hardkódovaná v kompilátoru
  * do budoucna se počítá s jejím vystavením a možnou modifikací (záměnou) uživatelem
  * to je ale v současnosti out-of-scope tohoto dokumentu

#### eapply
* implementace `eapply` vypadá následovně

~~~
(fn [abstraction arguments ranking-function]
    (letfn [
            (implemetation-arg-type 
                [implementation]
                (:arg-type (:lang-type (meta implementation))))
            (rank-implementations
                [v implementations]
                (map 
                    (fn [u] [(ranking-function (implementation-arg-type u)) u])
                    implementations))
            (select-implementation 
                [type abstraction]
                (get (reduce
                        (fn [x y] (if 
                                    (< (get x 0) (get y 0))
                                    x
                                    y)
                        (rank-implementations type abstraction)))))]
        (apply 
            (select-implementation (:lang-type (meta arguments)) abstraction) 
            arguments)))
~~~

### Construct
* kompilátor automaticky dosazuje odpovídající konstruktor jako anonymní funkci (je to moudré?)
* kompilátor potom vytvoří klasickou applikaci pomocí eapply a přeloží ji

Příklad:

~~~
(type Name)
(representation Unstructured Name)
(constructor Name:Unstructured ((String:Native name)) name)
(construct Name Unstructured "marvin")
 |
CLJ
 |
 V
(eapply
	(with-meta
		[(with-meta 
			(fn (name) (with-meta 
		      				name
		                	{:lang-type (lang-type-atom "Name" "Unstructured")}))
			{:lang-type (lang-type-arrow.
							[(lang-type-atom. "String" "Native")]
							(lang-type-atom. "Name" "Unstructured"))})]
		{:lang-type (lang-type-arrow.
							[(lang-type-atom. "String" "Native")]
							(lang-type-atom. "Name" "Unstructured"))})
    (with-meta
		[(with-meta
	        ["marvin"]
	        {:lang-type (lang-type-atom. "String" "Native")})]
		{:lang-type [(lang-type-atom. "String" "Native")]})
	ranking-function)
~~~

### DeconstructAs
* `deconstruct-as` je hloupý, typová informace dodaná uživatelem se používá při inferenci a kompilátor se spoléhá na její správnost (tedy, že je danou hodnotu možno dekonstruovat na daný typ)
* je zodpovědností uživatele specifikovat správný typ podle toho jak byla daná hodnota zkonstruována
* v tom uživateli může pomoc speciální forma `can-deconstruct-as` (viz. dále)
* co se clojure týče, `deconstruct-as` pouze vybalí hodnotu z obalovacího persistentVectoru

Příklad:

~~~
(deconstruct-as (construct Name Unstructured "marvin") String:Native)
 |
CLJ
 |
 V
(get 
    (eapply
		<konstrukční lambda výraz pro typ Name:Unstructured> 
		(with-meta
        	[(with-meta ["marvin"] {:lang-type (lang-type-atom. "String" "Native")})]
			{:lang-type [(lang-type-atom. "String" "Native")]})
		ranking-function) 
    0)
~~~

### CanDeconstructAs
* příjímá typ a hodnotu a vrací boolean
* indikuje, jestli je daná hodnota dekonstruovatelná na daný typ
* clojure to provádí jednoduchým srovnáním typu vnitřní hodnoty testované hodnoty se specifikovaným typem
* možná by bylo dobré přidat test a chybovou hlášku, pokud by se uživatel pokusil testovat hodnotu, která není složeným literálem atomického typu (tedy LitComposite)

Příklad:

~~~
(can-deconstruct-as Int:Native x)
 |
CLJ
 |
 V
(= 
    (lang-type-atom "Int" "Native")
    (:lang-type (meta (get x 0))))
~~~

### Convert
* podobně jako u konstrukce typu dosadí kompilátor anonymní funkci do běžného applikace v clojure
* o typy a metadata se stará samotné tělo konverze, uživatel je zodpovědný za to, že v konverzi zkonstruuje hodnotu odpovádajícího typu

Příklad:

~~~
(convert Int:Native Int:String 42)
 |
CLJ
 |
 V
((fn [x] (with-meta 
            [(Integer/toString (get x 0))]
            {:lang-type (lang-type-atom "Int" "String")}))
    (with-meta
        [42]
        {:lang-type (lang-type-atom "Int" "Native")}))
~~~

### And & Or
* speciální formy `and` a `or` se překládají přímo do svých protějšků v clojure
* jenom je nutné vybalit argumenty a zabalit návratovou hodnotu

~~~
(and #t #f)
 |
CLJ
 |
 V
(with-meta
    [(and
        (get (with-meta [true] {lang-type-atom "Bool" "Native"}) 0)
        (get (with-meta [false] {lang-type-atom "Bool" "Native"}) 0))]
    {:lang-type (lang-type-atom "Bool" "Native")})
~~~

### IfExpression
* je nutné vybalit pravdivostní hodnotu z podmínky

Příklad:

~~~
(if #t "yes" "no")
 |
CLJ
 |
 V
(if 
    (get (with-meta [true] {:lang-type (lang-type-atom "Bool" "Native")}) 0)
    (with-meta ["yes"] {:lang-type (lang-type-atom "String" "Native")})
    (with-meta ["no"] {:lang-type (lang-type-atom "String" "Native")}))
~~~

### ExceptionExpression
* vybalíme argument z obalového persistentVectoru
* k vyvolání vyjímky použijeme clojure konstrukci `(throw (Thorwable. \*))`

Příklad:

~~~
(error "message")
 |
CLJ
 |
 V
(throw (Throwable.
        (get
            (with-meta 
                ["message"] 
                {:lang-type (lang-type-atom "String" "Native")})
            0)))
~~~

### DefineSymbol
* používáme clojure construkci `def`

Příklad:

~~~
(define x 10) -CLJ-> (def x (with-meta 
                                [10] 
                                {:lang-type (lang-type-atom "Int" "Native)}))
~~~

### DefineType, DefineRepresentation, DefineConstructor, DefineConversion
* obsahují pouze informace pro kompilátor, při překladu do clojure se vynechávají
* kompilátor na odpovídající místa dodá anonymní funkce (viz. speciální forma Construct a Convert)
