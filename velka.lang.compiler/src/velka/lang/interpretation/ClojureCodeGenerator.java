package velka.lang.interpretation;

import java.util.Iterator;
import java.util.List;

import velka.lang.abstraction.ConversionOperators;
import velka.lang.expression.Expression;
import velka.lang.langbase.ListNative;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeTuple;
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
		try {
			return ClojureCodeGenerator.addTypeMetaInfo_str(cljCode, type.clojureTypeRepresentation());
		} catch (AppendableException e) {
			e.printStackTrace();
		}
		return "";
	}
	
	/**
	 * Adds type meta information to given clojure code piece
	 * 
	 * @param cljCode code to put the meta information to
	 * @param type    type
	 * @return clojure code with meta information
	 */
	public static String addTypeMetaInfo_str(String cljCode, String typeInfo) {
		StringBuilder sb = new StringBuilder();
		sb.append("(with-meta ");
		sb.append(cljCode);
		sb.append(" {:lang-type ");
		sb.append(typeInfo);
		sb.append("})");
		return sb.toString();
	}
	
	/**
	 * Symbol for type-2-type-symbol function
	 */
	public static String type2typeSymbolSymbol = "TYPE2TYPE-SYMBOL" + NameGenerator.next();
	
	/**
	 * Definition for type-2-type-symbol function
	 */
	public static String type2typeSymbolDef = 
			"(defn " + type2typeSymbolSymbol + " [type] \n" +
			ClojureCodeGenerator.addTypeMetaInfo_str("[type]", "type") + ")";

	/**
	 * Symbol for get-type clojure symbol
	 */
	public static String getTypeClojureSymbol = "GET-TYPE" + NameGenerator.next();

	/**
	 * Definition for get-type clojure symbol
	 */
	public static String getTypeClojureDef = "(defn " + getTypeClojureSymbol + " [expr] (:lang-type (meta expr)))";
	
	/**
	 * Symbol for tuple-2-velka-list function
	 */
	public static String tuple2velkaListSymbol = "TUPLE2VELKA-List" + NameGenerator.next();

	/**
	 * Definition for tuple-2-velka-list function
	 */
	public static String tuple2velkaListDef = 	"(defn " + tuple2velkaListSymbol + " [tuple] \n" + 
												"    (reduce \n" + 
												"        (fn [rest x] " + ClojureCodeGenerator.addTypeMetaInfo(
														"[" + 
												addTypeMetaInfo_str("[x rest]", "(velka.lang.types.TypeTuple. [(" + getTypeClojureSymbol + " x) " + TypeAtom.TypeListNative.clojureTypeRepresentation() + "])") +
														"]", TypeAtom.TypeListNative) + ") \n" + 
												"        " + ClojureCodeGenerator.addTypeMetaInfo("[" + 
														addTypeMetaInfo("[]", TypeTuple.EMPTY_TUPLE) +
														"]", TypeAtom.TypeListNative) + " (reverse tuple)))";

	/**
	 * Symbol for map of atomic type conversion
	 */
	public static String atomicConversionMapClojureSymbol = "*ATOMIC-CONVERSION-MAP*" + NameGenerator.next();
	
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
			"(def ^:dynamic " + atomicConversionMapClojureSymbol + 
				"{" + 
					makeAtomicConversionRecord(TypeAtom.TypeIntNative, TypeAtom.TypeIntString,
							ConversionOperators.IntNativeToIntString.clojureDef()) + "\n"
					+ makeAtomicConversionRecord(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman,
							ConversionOperators.IntNativeToIntRoman.clojureDef()) + "\n"
					+ makeAtomicConversionRecord(TypeAtom.TypeIntString, TypeAtom.TypeIntNative,
							ConversionOperators.IntStringToIntNative.clojureDef()) + "\n"
					+ makeAtomicConversionRecord(TypeAtom.TypeIntString, TypeAtom.TypeIntRoman,
							ConversionOperators.IntStringToIntRoman.clojureDef()) + "\n"
					+ makeAtomicConversionRecord(TypeAtom.TypeIntRoman, TypeAtom.TypeIntNative,
							ConversionOperators.IntRomanToIntNative.clojureDef()) + "\n"
					+ makeAtomicConversionRecord(TypeAtom.TypeIntRoman, TypeAtom.TypeIntString,
							ConversionOperators.IntRomanToIntString.clojureDef())
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
	 * Symbol for convert-set clojure function
	 */
	public static String convertRepOrClojureSymbol = "CONVERT-SET" + NameGenerator.next();
	/**
	 * Symbol for convert-to-set clojure function
	 */
	public static String convertToRepOrClojureSymbol = "CONVERT-TO-SET" + NameGenerator.next();
	/**
	 * Symbol for convert-from-set clojure function
	 */
	public static String convertFromRepOrClojureSymbol = "CONVERT-FROM-SET" + NameGenerator.next();
	/**
	 * Symbol for convert clojure function
	 */
	public static String convertClojureSymbol = "CONVERT" + NameGenerator.next();
	
	/**
	 * Definition for convert-type-atom clojure function
	 */
	public static String convertAtomClojureDef = 
			"(defn " + convertAtomClojureSymbol + " [to arg]\n" +
			"    (let [from (" + getTypeClojureSymbol + " arg)]\n" +
			"        (cond (= from to) arg\n" + 
			"              (and (instance? velka.lang.types.TypeAtom to) (= (.representation to) velka.lang.types.TypeRepresentation/WILDCARD)) arg\n" +
			"              (contains? " + atomicConversionMapClojureSymbol + " [from to])\n" +
			"                  (((get " + atomicConversionMapClojureSymbol + " [from to]) nil) arg)\n" + 
			"              :else (throw (Throwable. (str \"Conversion from \" from \" to \" to \" does not exists.\"))))))";
	
	/**
	 * Definition for convert-tuple clojure function
	 */
	public static String convertTupleClojureDef = 
			"(defn " + convertTupleClojureSymbol + " [to arg]\n" +
			addTypeMetaInfo_str(
			"    (vec (map " + convertClojureSymbol + " to arg))",
			"to") + ")";

	/**
	 * Definition for convert-fn clojure function
	 */
	public static String convertFnClojureDef = 
			"(defn " + convertFnClojureSymbol + " [to arg]\n" +
			"    (let [from (" + getTypeClojureSymbol + " arg)\n" +
			"          impl " + addTypeMetaInfo_str("(fn [& a] (" + convertClojureSymbol + " (.rtype to)\n" +
			"                                (apply (arg nil) " + 
			"										(" + convertClojureSymbol + 
			" 											(.ltype from) " + 
														addTypeMetaInfo_str("a", "(.ltype to)") + "))))", "to") + "]\n" +
			addTypeMetaInfo_str(
			"        (fn ([args] impl)\n" +
			"            ([args ranking-fn] impl))", "to") + "))";
	
	/**
	 * Definition for convert-set clojure function
	 */
	public static String convertRepOrClojureDef = 
			"(defn " + convertRepOrClojureSymbol + " [to arg]\n" + 
			"	(let [from (" + getTypeClojureSymbol + " arg)\n" +
			"		  reps (.getRepresentations from)]\n" +
			"		(if (some identity \n" + //~ (apply or...
			"				(map (fn [t] (try (velka.lang.types.Type/unifyTypes t to) (catch velka.lang.types.TypesDoesNotUnifyException e false)))\n" +
			"					reps))\n" +
			"			arg (throw (Throwable. (str \"Conversion from \" from \" to \" to \" does not exists.\"))))))";
	
	/**
	 * Definition for convert-to-set clojure function
	 */
	public static String convertToRepOrClojureDef = 
			"(defn " + convertToRepOrClojureSymbol + " [to arg]\n" + 
					"	(let [from (" + getTypeClojureSymbol + " arg)\n" +
					"		  reps (.getRepresentations to)]\n" +
					"		(if (some identity \n" + //~ (apply or...
					"				(map (fn [t] (try (velka.lang.types.Type/unifyTypes t from) (catch velka.lang.types.TypesDoesNotUnifyException e false)))\n" +
					"					reps))\n" +
					"			arg (throw (Throwable. (str \"Conversion from \" from \" to \" to \" does not exists.\"))))))";
	
	/**
	 * Definition for convert clojure function
	 */
	public static String convertClojureDef = 
			"(defn " + convertClojureSymbol + " [to arg]\n" +
			"    (let [from (" + getTypeClojureSymbol + " arg)]" +
			"        (if (instance? velka.lang.types.TypeVariable to)\n" +
			"            arg\n" +
			"            (if (instance? velka.lang.types.RepresentationOr to)\n" +
			"				(" + convertToRepOrClojureSymbol + " to arg)\n" + 
			"            	(cond (instance? velka.lang.types.TypeAtom from) (" + convertAtomClojureSymbol + " to arg)\n" + 
			"                 	  (instance? velka.lang.types.TypeTuple from) (" + convertTupleClojureSymbol + " to arg)\n" +
			"                  	  (instance? velka.lang.types.TypeArrow from) (" + convertFnClojureSymbol + " to arg)\n" + 
			"                  	  (instance? velka.lang.types.RepresentationOr from) (" + convertRepOrClojureSymbol + " to arg))))))";
	
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
			"                                 (.ltype (" + getTypeClojureSymbol + " impl))\n" + 
			"                                 args)]\n" +
			"            (apply impl converted-args)))\n" + 
			"    ([abstraction args]\n" + 
			"        (let [impl (abstraction args)\n" +
			"              converted-args (" + convertClojureSymbol + "\n" +
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
			"    (let [args-type (" + tuple2velkaListSymbol + "\n" +  
			"                        (map " + type2typeSymbolSymbol +" (" + getTypeClojureSymbol + " args)))]\n" + 
			"        (get \n" +
			"            (reduce \n" +
			"                (fn [x y] (if (< (get x 0) (get y 0)) x y))\n" +
			"                (map \n" + 
			"                    (fn [impl] [\n" + 
			"                        (first (" + eapplyClojureSymbol + "\n" +
			"                                  ranking-fn\n" +
			addTypeMetaInfo(
			"                                  [(" + tuple2velkaListSymbol + "\n" + 
			"                                       (map " + type2typeSymbolSymbol + 
			"                                                (.ltype (" + getTypeClojureSymbol + " impl)))) \n" + 
			"                                   args-type]", new TypeTuple(TypeAtom.TypeListNative, TypeAtom.TypeListNative)) + "))\n"+
			"                        impl])\n" + 
			"                    impls))\n" +
			"        1)))";
	
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
		sb.append("(require '[clojure.string])\n");
		sb.append("(ns clojure.examples.hello (:gen-class))");
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.type2typeSymbolSymbol));
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.tuple2velkaListSymbol));
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.getTypeClojureSymbol));
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.atomicConversionMapClojureSymbol));
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.convertAtomClojureSymbol));
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.convertTupleClojureSymbol));
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.convertFnClojureSymbol));
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.convertRepOrClojureSymbol));
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.convertToRepOrClojureSymbol));
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.convertClojureSymbol));
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.selectImplementationClojureSymbol));
		sb.append(ClojureCodeGenerator.makeDeclaration(ClojureCodeGenerator.eapplyClojureSymbol));
		
		sb.append(type2typeSymbolDef);
		sb.append("\n");
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
		sb.append(convertRepOrClojureDef);
		sb.append("\n");
		sb.append(convertToRepOrClojureDef);
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
	
	/**
	 * Writes footers
	 * @remark  creates main entry point for clojure applictation calling function main with no arguments
	 * @return string with code
	 */
	public static String writeFooters() {
		//TODO add support of command line arguments!
		return "(defn -main\n" + 
				"  []\n" + 
				"  (EAPPLYSYSGENNAMEgx main (with-meta [] {:lang-type (velka.lang.types.TypeTuple/EMPTY_TUPLE)})))";
	}
}
