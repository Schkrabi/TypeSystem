# Test Case
Základní problém se projevil při testování seznamů. Podařilo se mi vytvořit následující (minimální?) testcase:

~~~
(define foo (lambda ((A x)) x))
(define bar (lambda ((A y)) (foo 42)))
(bar "s")
~~~

Tento kód vyvolá `TypesDoesNotUnifyException` s textem `Types String:Native and Int:Native does not unifyin (bar ["bar"])`.

* Problém tkví v tom, že při aplikaci `(bar "s")` se na proměnnou `x` naváže hodnota `"s"` a tedy se (správně odvodí) substituce `{A:=String:Native}`
* Poté proběhne applikace `(foo 42)`, kde se na proměnnou `x` v funkci `foo` naváže `42` a tedy se správně odvodí substituce `{A:=Int:Native}`
* Když se potom inference vrací do funkce `bar` pokusí se algoritmus sjendotit substituce a dojde ke konfliktu

~~~
A:=String:Native
A:=Int:Native
~~~
* Algoritmus se tedy pokusí sjednotit navázání dvou typových proměnných z různých abstrakcí
* Problém se tedy zdá být v absenci lexikálního uzávěru v rámci infernce abstrakce pro typové proměnné

# Řešení
* Problém se týká pouze výrazů, kde může typy specifikovat uživatel, tedy následujících:
    * `can-deconstruct-as`
    * `conversion`
    * `convert`
    * `constructor`
    * `construct`
    * `deconstruct`
    * `lambda`
    * `extended-lambda`
* diskuse úprav jednotlivých výrazů následuje

## `conversion`, `constructor`, `convert`, `construct`
* problém by se projevovat neměl, uživatlé zde mohou specifikovat pouze typové atomy
* pro speciální formu `construct` je možná tohle omezení zbytečně striktní -> přejmenovat speciální formu na `convertAtom`?

## `deconstruct`
* při interpretaci problém nenastává, forma se pokusí unifikovat vnitřní typ argumentu se specifikovaným typem, ale substituce se zahodí
* při překladu do clojure by taky problém nastat neměl, forma jenom vybalí hodnotu z obalovacího persistentVectoru
* nicméně při inferenci se vrací v typu uživatelem specifikovaný typ, tedy může dojít ke kolizím
* ale tyto kolize mohou být záměrné, uvažujme příklad:

~~~
    (define foo 
        (lambda ((A x)) 
            (deconstruct (construct Int Roman "XLII") A)))
    (foo "x") => "XLII" : Int:Roman ;;Tohle jsem očekával
    (foo 0) => "XLII" : Int:Roman ;;Překvapivě to nehodí chybu
~~~

* tohle umožňuje uživatelovi nepřímo předávat typy jako argumenty! Chceme to?
* Když připustíme, že typová proměnná by měla mít uzávěr stejně jako lexikální uzávěr běžných proměnných tak jak bychom se měli chovat v tomto případě?

## `can-deconstruct-as`
* stejný problém jako u `deconstruct`
* V překladu do clojure se bude muset nahradit `=` za unifikační algoritmus

## `lambda` a `extended-lambda`
* tady celý problém začal, nejlogičtější řešení by se mi zdálo zavést něco na způsob lexikálního uzávěru pro typové proměnné, otázka je jak to udělat technicky?
* byla by tu možnost při sémantickém parsování proscanovat každý lambda výraz a nahradit uživatelem specifikované typové proměnné vygenerovanými unikátními identifikátory* popřípadě nějakým způsobem modifikovat inferenční algoritmus například:
    * při inferenci lambda (nebo extended-lambda) výrazu zaznamenat typové argumenty použité v argumentech funkce
    * při vracení  typu a substituce v inferenci lambdy smazat ve vracené substituci pravidla, která obsahují tyto typové proměnné
    
# Závěr
* po konzultaci s Petrem Krajčou jsem se rozhodl podmínit použítí typových proměnných jejich deklarací v rámci speciální formy `let-type`
* to umožní překladači transparentně přejmenovat typové proměnné v různých scopech a zároveň uživatel explicitně uvidí v jakém scope je ta která typová proměnná platná
* co se týče creepu odkazovaní typů přes typové proměnné, rovněž po konzultaci s Petrem Krajčou, jsem se rozhodl zaznamenat v dokumentaci toto chování a odrazovat od něj uživatele, podle zásady, pokud se programátor chce střelit do nohy, nechme ho
