package util;

import java.io.IOException;
import java.io.Writer;
import java.util.Iterator;
import java.util.List;

import expression.Expression;

public class ClojureCodeGenerator {
	public static void toClojureCode(List<Expression> exprs, Writer target) throws IOException, Exception {
		ClojureCodeGenerator.writeHeaders(target);

		Iterator<Expression> i = exprs.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			target.write(e.toClojureCode());
			if (i.hasNext()) {
				target.write('\n');
			}
		}
	}

	private static void writeHeaders(Writer target) throws IOException {
		target.write("(defrecord lang-type-atom [name representation])\n");
		target.write("(defrecord lang-type-arrow [arg-type return-type])\n");
		target.write("(def lang-pstr " + "(fn [exp] " + "(letfn [(lang-pstr-aux [exp level] "
				+ "(let [type (:lang-type (meta exp))] " + "(cond " + "(or "
				+ "(= type (lang-type-atom. \"Int\" \"Native\")) " + "(= type (lang-type-atom. \"String\" \"Native\")) "
				+ "(= type (lang-type-atom. \"Double\" \"Native\")) "
				+ "(= type (lang-type-atom. \"Bool\" \"Native\"))) (if " + "(= level 0) " + "(pr-str (get exp 0)) "
				+ "(get exp 0)) " + "(= type []) []"
				+ "(instance? lang-type-atom type) (lang-pstr-aux (get exp 0) level) "
				+ "(instance? clojure.lang.PersistentVector type) " + "(if " + "(= level 0) "
				+ "(pr-str (vec (map (fn [x] (lang-pstr-aux x (+ level 1))) exp))) "
				+ " (vec (map (fn [x] (lang-pstr-aux x (+ level 1))) exp))) "
				+ ":else (throw (Throwable. (str (pr-str exp) \" is not a printable expression\"))))))] "
				+ "(lang-pstr-aux exp 0))))");
	}
}
