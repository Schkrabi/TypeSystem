package velka.lang.interpretation;

import java.util.Iterator;
import java.util.List;

import velka.lang.abstraction.Operator;
import velka.lang.expression.Expression;
import velka.lang.langbase.ListNative;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.util.AppendableException;
import velka.lang.util.NameGenerator;

public class ClojureCodeGenerator {

	/**
	 * Adds type meta information to given clojure code piece
	 * 
	 * @param cljCode code to put the meta information to
	 * @param type    type
	 * @return clojure code with meta information
	 */
	public static String addTypeMetaInfo(String cljCode, Type type) {
		StringBuilder sb = new StringBuilder();
		sb.append("(with-meta ");
		sb.append(cljCode);
		sb.append(" {:lang-type ");
		try {
			sb.append(type.clojureTypeRepresentation());
		} catch (AppendableException e) {
			e.printStackTrace();
		}
		sb.append("})");
		return sb.toString();
	}

	/**
	 * Symbol for tuple-2-velka-list function
	 */
	public static String tuple2velkaListSymbol = "TUPLE2VELKA-List" + NameGenerator.next();

	/**
	 * Definition for tuple-2-velka-list function
	 */
	public static String tuple2velkaListDef = 	"(defn " + tuple2velkaListSymbol + " [tuple] \n" + 
												"    (reduce \n" + 
												"        (fn [rest x] " + ClojureCodeGenerator.addTypeMetaInfo("[x rest]", TypeAtom.TypeListNative) + ") \n" + 
												"        " + ClojureCodeGenerator.addTypeMetaInfo("[]", TypeAtom.TypeListNative) + " (reverse tuple)))";

	/**
	 * Symbol for get-type clojure symbol
	 */
	public static String getTypeClojureSymbol = "GET-TYPE" + NameGenerator.next();

	/**
	 * Definition for get-type clojure symbol
	 */
	public static String getTypeClojureDef = "(defn " + getTypeClojureSymbol + " [expr] (:lang-type (meta expr)))";

	/**
	 * Symbol for map of atomic type conversion
	 */
	public static String atomicConversionMapClojureSymbol = "ATOMIC-CONVERSION-MAP" + NameGenerator.next();
	
	/**
	 * Creates record for atomic conversion map
	 * @param fromType
	 * @param toType
	 * @param conversionCode
	 * @return
	 */
	public static String makeAtomicConversionRecord(TypeAtom fromType, TypeAtom toType, String conversionCode) {
		StringBuilder sb = new StringBuilder();
		sb.append("[");
		sb.append(fromType.clojureTypeRepresentation());
		sb.append(" ");
		sb.append(toType.clojureTypeRepresentation());
		sb.append("]");
		sb.append(" ");
		sb.append(conversionCode);
		return sb.toString();
	}
	
	/**
	 * Definition for map of atomic type conversion
	 */
	public static String atomicConversionMapClojureDef = 
			"(def " + atomicConversionMapClojureSymbol + 
				"{" + 
					makeAtomicConversionRecord(TypeAtom.TypeIntNative, TypeAtom.TypeIntString,
							Operator.IntNativeToIntString.clojureDef()) + "\n"
					+ makeAtomicConversionRecord(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman,
							Operator.IntNativeToIntRoman.clojureDef()) + "\n"
					+ makeAtomicConversionRecord(TypeAtom.TypeIntString, TypeAtom.TypeIntNative,
							Operator.IntStringToIntNative.clojureDef()) + "\n"
					+ makeAtomicConversionRecord(TypeAtom.TypeIntString, TypeAtom.TypeIntRoman,
							Operator.IntStringToIntRoman.clojureDef()) + "\n"
					+ makeAtomicConversionRecord(TypeAtom.TypeIntRoman, TypeAtom.TypeIntNative,
							Operator.IntRomanToIntNative.clojureDef()) + "\n"
					+ makeAtomicConversionRecord(TypeAtom.TypeIntRoman, TypeAtom.TypeIntString,
							Operator.IntRomanToIntString.clojureDef())
					+
				"})";
	
	/**
	 * Symbol for convert-type-atom clojure function
	 */
	public static String convertAtomClojureSymbol = "CONVERT-TYPE-ATOM" + NameGenerator.next();
	/**
	 * Symbol for convert-tuple clojure symbol
	 */
	public static String convertTupleClojureSymbol = "CONVERT-TUPLE" + NameGenerator.next();
	/**
	 * Symbol for convert-fn clojure function
	 */
	public static String convertFnClojureSymbol = "CONVERT-FN" + NameGenerator.next();
	/**
	 * Symbol for convert clojure function
	 */
	public static String convertClojureSymbol = "CONVERT" + NameGenerator.next();
	
	/**
	 * Definition for convert-type-atom clojure function
	 */
	public static String convertAtomClojureDef = 
			"(defn " + convertAtomClojureSymbol + " [from to arg]\n" +
			"    (((get " + atomicConversionMapClojureSymbol + " [from to]) nil) arg))";
	
	/**
	 * Definition for convert-tuple clojure function
	 */
	public static String convertTupleClojureDef = 
			"(defn " + convertTupleClojureSymbol + " [from to arg]\n" +
			"    (vec (map " + convertClojureSymbol + " from to arg)))";

	/**
	 * Definition for convert-fn clojure function
	 */
	public static String convertFnClojureDef = 
			"(defn " + convertFnClojureSymbol + " [from to arg]\n" +
			"    (let [impl (fn [& a] (" + convertClojureSymbol + " (.rtype from) (.rtype to)\n" +
			"                                (apply (arg nil) (" + convertClojureSymbol + " (.ltype to) (.ltype from) a))))]\n" +
			"        (fn ([args] impl)\n" +
			"            ([args ranking-fn] impl))))";
	
	/**
	 * Definition for convert clojure function
	 */
	public static String convertClojureDef = 
			"(defn " + convertClojureSymbol + " [from to arg]\n" +
			"        (if (or (instance? velka.lang.types.TypeVariable to)\n" +
			"                (= from to))\n" +
			"            arg\n" +
			"            (cond (instance? velka.lang.types.TypeAtom from) (" + convertAtomClojureSymbol + " from to arg)\n" + 
			"                  (instance? velka.lang.types.TypeTuple from) (" + convertTupleClojureSymbol + " from to arg)\n" +
			"                  (instance? velka.lang.types.TypeArrow from) (" + convertFnClojureSymbol + " from to arg)\n" + 
			"                  (instance? velka.lang.types.RepresentationOr from) (throw (Throwable. \"trying to convert representationor\")))))";
	
	/**
	 * Symbol for eapply function
	 */
	public static String eapplyClojureSymbol = "EAPPLY" + NameGenerator.next();
	
	/**
	 * Definition for eapply function
	 */
	public static String eapplyClojureDef = 
			"(defn " + eapplyClojureSymbol + "\n" +
			"    ([abstraction args ranking]\n" +
			"        (let [impl (abstraction args ranking)\n" +
			"              converted-args (" + convertClojureSymbol + "\n" + 
			"                                 (" + getTypeClojureSymbol + " args)\n" + 
			"                                 (.ltype (" + getTypeClojureSymbol + " impl))\n" + 
			"                                 args)]\n" +
			"            (apply impl converted-args)))\n" + 
			"    ([abstraction args]\n" + 
			"        (let [impl (abstraction args)\n" +
			"              converted-args (" + convertClojureSymbol + "\n" +
			"                                 (" + getTypeClojureSymbol + " args)\n" + 
			"                                 (.ltype (" + getTypeClojureSymbol + " impl))\n" + 
			"                                 args)]\n" +
			"            (apply impl converted-args))))";
	
	/**
	 * Symbol for select-implementation-clojure function
	 */
	public static String selectImplementationClojureSymbol = "SELECT-IMPLEMENTATION" + NameGenerator.next();

	/**
	 * Definition for select-implementation-clojure function
	 */
	public static String selectImplementationClojureDef = 
			"(defn " + selectImplementationClojureSymbol + "\n" + 
			"    [ranking-fn args impls] \n" + 
			"    (let [args-type (" + tuple2velkaListSymbol + " (" + getTypeClojureSymbol + " args))]\n" + 
			"        (get \n" +
			"            (reduce \n" +
			"                (fn [x y] (if (< (get x 0) (get y 0)) x y))\n" +
			"                (map \n" + 
			"                    (fn [impl] [\n" + 
			"                        (" + eapplyClojureSymbol + "\n" +
			"                            ranking-fn\n" +
			"                            [(" + tuple2velkaListSymbol + " (.ltype (" + getTypeClojureSymbol + " impl)))])\n"+
			"                        impl])\n" + 
			"                    impls))\n" +
			"        0)))";
	
	private static String makeDeclaration(String symbol) {
		return "(declare " + symbol + ")";
	}

	public static String toClojureCode(List<Expression> exprs, Environment env, TypeEnvironment typeEnv)
			throws Exception {
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
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.tuple2velkaListSymbol));
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.getTypeClojureSymbol));
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.atomicConversionMapClojureSymbol));
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.convertAtomClojureSymbol));
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.convertTupleClojureSymbol));
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.convertFnClojureSymbol));
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.convertClojureSymbol));
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.selectImplementationClojureSymbol));
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.eapplyClojureSymbol));
		
		sb.append(tuple2velkaListDef);
		sb.append("\n");
		sb.append(getTypeClojureDef);
		sb.append("\n");
		sb.append(atomicConversionMapClojureDef);
		sb.append("\n");
		sb.append(convertAtomClojureDef);
		sb.append("\n");
		sb.append(convertTupleClojureDef);
		sb.append("\n");
		sb.append(convertFnClojureDef);
		sb.append("\n");
		sb.append(convertClojureDef);
		sb.append("\n");
		sb.append(selectImplementationClojureDef);
		sb.append("\n");
		sb.append(eapplyClojureDef);
		sb.append("\n");
		sb.append("(def lang-pstr" + "(fn [exp]" + "(letfn [(lang-pstr-aux [exp level]"
				+ "(let [type (:lang-type (meta exp))]" + "(cond" + "(or"
				+ "(= type velka.lang.types.TypeAtom/TypeIntNative)"
				+ "(= type velka.lang.types.TypeAtom/TypeStringNative)"
				+ "(= type velka.lang.types.TypeAtom/TypeDoubleNative)"
				+ "(= type velka.lang.types.TypeAtom/TypeBoolNative))" + "(if (= level 0)" + "(pr-str (get exp 0))"
				+ "(get exp 0))" + "(= type velka.lang.types.TypeTuple/EMPTY_TUPLE) []"
				+ "(instance? velka.lang.types.TypeAtom type) (lang-pstr-aux (get exp 0) level)"
				+ "(instance? velka.lang.types.TypeTuple type) " + "(if" + "(= level 0)"
				+ "(pr-str (vec (map (fn [x] (lang-pstr-aux x (+ level 1))) exp)))"
				+ "(vec (map (fn [x] (lang-pstr-aux x (+ level 1))) exp)))"
				+ ":else (throw (Throwable. (str exp \" is not a printable expression\"))))))]"
				+ "(lang-pstr-aux exp 0))))");
		sb.append("\n");
		sb.append(ListNative.makeClojureCode(env, typeEnv));
		sb.append("\n\n");
		return sb.toString();
	}
}
