package velka.lang.interpretation;

import java.io.IOException;
import java.io.Writer;
import java.util.Iterator;
import java.util.List;

import velka.lang.expression.Expression;
import velka.lang.langbase.ListNative;

public class ClojureCodeGenerator {
	public static void toClojureCode(List<Expression> exprs, Writer target, Environment env, TypeEnvironment typeEnv) throws IOException, Exception {
		ClojureCodeGenerator.writeHeaders(target, env, typeEnv);

		Iterator<Expression> i = exprs.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			target.write(e.toClojureCode(env, typeEnv));
			if (i.hasNext()) {
				target.write('\n');
			}
		}
	}

	private static void writeHeaders(Writer target, Environment env, TypeEnvironment typeEnv) throws IOException {
		target.write("(def lang-pstr\n" + 
				"    (fn [exp]\n" + 
				"        (letfn [(lang-pstr-aux [exp level]\n" + 
				"                    (let [type (:lang-type (meta exp))]\n" + 
				"                        (cond\n" + 
				"                            (or \n" + 
				"                                (= type velka.lang.types.TypeAtom/TypeIntNative)\n" + 
				"                                (= type velka.lang.types.TypeAtom/TypeStringNative) \n" + 
				"                                (= type velka.lang.types.TypeAtom/TypeDoubleNative) \n" + 
				"                                (= type velka.lang.types.TypeAtom/TypeBoolNative)) \n" + 
				"                                    (if (= level 0) \n" + 
				"                                        (pr-str (get exp 0)) \n" + 
				"                                        (get exp 0))\n" + 
				"                            (= type velka.lang.types.TypeTuple/EMPTY_TUPLE) []\n" + 
				"                            (instance? velka.lang.types.TypeAtom type) (lang-pstr-aux (get exp 0) level)\n" + 
				"                            (instance? velka.lang.types.TypeTuple type) \n" + 
				"                            (if \n" + 
				"                                (= level 0) \n" + 
				"                                (pr-str (vec (map (fn [x] (lang-pstr-aux x (+ level 1))) exp)))\n" + 
				"                                (vec (map (fn [x] (lang-pstr-aux x (+ level 1))) exp)))\n" + 
				"                            :else (throw (Throwable. (str exp \" is not a printable expression\"))))))]\n" + 
				"            (lang-pstr-aux exp 0))))");
		target.write(ListNative.makeClojureCode(env, typeEnv));
	}
}
