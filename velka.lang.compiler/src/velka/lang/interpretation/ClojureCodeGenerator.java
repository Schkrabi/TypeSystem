package velka.lang.interpretation;

import java.util.Iterator;
import java.util.List;

import velka.lang.expression.Expression;
import velka.lang.langbase.ListNative;

public class ClojureCodeGenerator {
	public static String toClojureCode(List<Expression> exprs, Environment env, TypeEnvironment typeEnv) throws Exception {
		StringBuilder sb = new StringBuilder(); 

		Iterator<Expression> i = exprs.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			sb.append(e.toClojureCode(env, typeEnv));
			if (i.hasNext()) {
				sb.append('\n');
			}
		}
		return sb.toString();
	}

	public static String writeHeaders(Environment env, TypeEnvironment typeEnv) {
		StringBuilder sb = new StringBuilder();
		sb.append("(def lang-pstr" + 
					"(fn [exp]" + 
						"(letfn [(lang-pstr-aux [exp level]" + 
							"(let [type (:lang-type (meta exp))]" + 
								"(cond" + 
									"(or" + 
										"(= type velka.lang.types.TypeAtom/TypeIntNative)" + 
										"(= type velka.lang.types.TypeAtom/TypeStringNative)" + 
										"(= type velka.lang.types.TypeAtom/TypeDoubleNative)" + 
										"(= type velka.lang.types.TypeAtom/TypeBoolNative))" + 
											"(if (= level 0)" + 
												"(pr-str (get exp 0))" + 
												"(get exp 0))" + 
												"(= type velka.lang.types.TypeTuple/EMPTY_TUPLE) []" + 
									"(instance? velka.lang.types.TypeAtom type) (lang-pstr-aux (get exp 0) level)" + 
									"(instance? velka.lang.types.TypeTuple type) " + 
										"(if" + 
											"(= level 0)" + 
											"(pr-str (vec (map (fn [x] (lang-pstr-aux x (+ level 1))) exp)))" + 
											"(vec (map (fn [x] (lang-pstr-aux x (+ level 1))) exp)))" + 
									":else (throw (Throwable. (str exp \" is not a printable expression\"))))))]" + 
							"(lang-pstr-aux exp 0))))");
		sb.append(ListNative.makeClojureCode(env, typeEnv));
		return sb.toString();
	}
}
