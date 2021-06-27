package velka.lang.interpretation;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import velka.lang.abstraction.ConversionOperators;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeTuple;

/**
 * This class is generating velka.clojure.core namespace file velka/clojure/core.clj
 * @author Mgr. Radomir Skrabal
 *
 */
public class VelkaClojureCore {

	/**
	 * Namespace for velka.clojure.core
	 */
	public static String NAMESPACE = "velka.clojure.core";
	/**
	 * Symbol for type-2-type-symbol function
	 */
	private static String type2typeSymbolSymbol = "type-2-typesymbol";
	/**
	 * Fully qualified type-2-type-symbol function symbol
	 */
	public static String type2typeSymbolSymbol_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, type2typeSymbolSymbol);
	/**
	 * Symbol for get-type clojure symbol
	 */
	static String getTypeClojureSymbol = "get-type";
	/**
	 * Fully qualified get-type clojure symbol
	 */
	public static String getTypeClojureSymbol_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, getTypeClojureSymbol);
	/**
	 * Symbol for tuple-2-velka-list function
	 */
	static String tuple2velkaListSymbol = "tuple-2-list";
	/**
	 * Fully qualified tuple-2-velka-list symbol
	 */
	public static String tuple2velkaListSymbol_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, tuple2velkaListSymbol);
	/**
	 * Symbol for map of atomic type conversion
	 */
	private static String atomicConversionMapClojureSymbol = "*ATOMIC-CONVERSION-MAP*";
	/**
	 * Fully qualified atomic-conversion-map symbol
	 */
	public static String atomicConversionMapClojureSymbol_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, atomicConversionMapClojureSymbol);
	/**
	 * Symbol for convert-type-atom clojure function
	 */
	static String convertAtomClojureSymbol = "convert-type-atom";
	/**
	 * Fully qualified convert-type-atom clojure symbol
	 */
	public static String convertAtomClojureSymbol_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, convertAtomClojureSymbol);
	/**
	 * Symbol for convert-tuple clojure symbol
	 */
	private static String convertTupleClojureSymbol = "convert-tuple";
	/**
	 * Fully qualified convert-tuple clojure symbol
	 */
	public static String convertTupleClojureSymbol_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, convertTupleClojureSymbol);
	/**
	 * Symbol for convert-fn clojure function
	 */
	private static String convertFnClojureSymbol = "convert-fn";
	/**
	 * Fully qualified convert-fn symbol
	 */
	public static String convertFnClojureSymbol_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, convertFnClojureSymbol);
	/**
	 * Symbol for convert-set clojure function
	 */
	private static String convertRepOrClojureSymbol = "convert-set";
	/**
	 * Fully qualified symbol convert-set
	 */
	public static String convertRepOrClojureSymbol_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, convertRepOrClojureSymbol);
	/**
	 * Symbol for convert-to-set clojure function
	 */
	private static String convertToRepOrClojureSymbol = "convert-to-set";
	/**
	 * Fully qualified symbol convert-to-set
	 */
	public static String convertToRepOrClojureSymbol_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, convertToRepOrClojureSymbol);
	/**
	 * Symbol for convert-from-set clojure function
	 */
	static String convertFromRepOrClojureSymbol = "convert-from-set";
	/**
	 * Fully qualified symbol convert-from-set
	 */
	public static String convertFromRepOrClojureSymbol_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, convertFromRepOrClojureSymbol);
	/**
	 * Symbol for convert clojure function
	 */
	private static String convertClojureSymbol = "convert";
	/**
	 * Fully qualified symbol convert
	 */
	public static String convertClojureSymbol_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, convertClojureSymbol);
	/**
	 * Symbol for eapply function
	 */
	private static String eapplyClojureSymbol = "eapply";
	/**
	 * Fully qualified eapply symbol
	 */
	public static String eapplyClojureSymbol_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, eapplyClojureSymbol);
	/**
	 * Symbol for select-implementation-clojure function
	 */
	private static String selectImplementationClojureSymbol = "select-implementation";
	/**
	 * Fully qualify select-implementation-clojure symbol
	 */
	public static String selectImplementationClojureSymbol_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, selectImplementationClojureSymbol);
	/**
	 * Definition for type-2-type-symbol function
	 */
	public static String type2typeSymbolDef = "(defn " + type2typeSymbolSymbol + " [type] \n"
			+ ClojureHelper.addTypeMetaInfo_str("[type]", "type") + ")";
	/**
	 * Definition for get-type clojure symbol
	 */
	public static String getTypeClojureDef = "(defn " + getTypeClojureSymbol + " [expr] (:lang-type (meta expr)))";
	/**
	 * Definition for tuple-2-velka-list function
	 */
	public static String tuple2velkaListDef = "(defn " + tuple2velkaListSymbol + " [tuple] \n" + "    (reduce \n"
			+ "        (fn [rest x] "
			+ ClojureHelper.addTypeMetaInfo(
					"[" + ClojureHelper.addTypeMetaInfo_str("[x rest]",
							"(velka.lang.types.TypeTuple. [(" + getTypeClojureSymbol + " x) "
									+ TypeAtom.TypeListNative.clojureTypeRepresentation() + "])")
							+ "]",
					TypeAtom.TypeListNative)
			+ ") \n" + "        " + ClojureHelper
					.addTypeMetaInfo("[" + ClojureHelper.addTypeMetaInfo("[]", TypeTuple.EMPTY_TUPLE) + "]", TypeAtom.TypeListNative)
			+ " (reverse tuple)))";
	/**
	 * Definition for map of atomic type conversion
	 */
	public static String atomicConversionMapClojureDef = "(def ^:dynamic " + atomicConversionMapClojureSymbol + "{"
			+ makeAtomicConversionRecord(TypeAtom.TypeIntNative, TypeAtom.TypeIntString,
					ConversionOperators.IntNativeToIntString.clojureDef())
			+ "\n"
			+ makeAtomicConversionRecord(
					TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman, ConversionOperators.IntNativeToIntRoman.clojureDef())
			+ "\n"
			+ makeAtomicConversionRecord(TypeAtom.TypeIntString, TypeAtom.TypeIntNative,
					ConversionOperators.IntStringToIntNative.clojureDef())
			+ "\n"
			+ makeAtomicConversionRecord(
					TypeAtom.TypeIntString, TypeAtom.TypeIntRoman, ConversionOperators.IntStringToIntRoman.clojureDef())
			+ "\n"
			+ makeAtomicConversionRecord(TypeAtom.TypeIntRoman, TypeAtom.TypeIntNative,
					ConversionOperators.IntRomanToIntNative.clojureDef())
			+ "\n" + makeAtomicConversionRecord(TypeAtom.TypeIntRoman, TypeAtom.TypeIntString,
					ConversionOperators.IntRomanToIntString.clojureDef())
			+ "})";
	/**
	 * Definition for convert-type-atom clojure function
	 */
	public static String convertAtomClojureDef = "(defn " + convertAtomClojureSymbol + " [to arg]\n"
			+ "    (let [from (" + getTypeClojureSymbol + " arg)]\n" + "        (cond (= from to) arg\n"
			+ "              (and (instance? velka.lang.types.TypeAtom to) (= (.representation to) velka.lang.types.TypeRepresentation/WILDCARD)) arg\n"
			+ "              (contains? " + atomicConversionMapClojureSymbol + " [from to])\n"
			+ "                  (((get " + atomicConversionMapClojureSymbol + " [from to]) nil) arg)\n"
			+ "              :else (throw (Throwable. (str \"Conversion from \" from \" to \" to \" does not exists.\"))))))";
	/**
	 * Definition for convert-tuple clojure function
	 */
	public static String convertTupleClojureDef = "(defn " + convertTupleClojureSymbol + " [to arg]\n"
			+ ClojureHelper.addTypeMetaInfo_str("    (vec (map " + convertClojureSymbol + " to arg))", "to") + ")";
	/**
	 * Definition for convert-fn clojure function
	 */
	public static String convertFnClojureDef = "(defn " + convertFnClojureSymbol + " [to arg]\n" + "    (let [from ("
			+ getTypeClojureSymbol + " arg)\n" + "          impl "
			+ ClojureHelper.addTypeMetaInfo_str("(fn [& a] (" + convertClojureSymbol + " (.rtype to)\n"
					+ "                                (apply (arg nil) " + "										("
					+ convertClojureSymbol + " 											(.ltype from) "
					+ ClojureHelper.addTypeMetaInfo_str("a", "(.ltype to)") + "))))", "to")
			+ "]\n" + ClojureHelper.addTypeMetaInfo_str("        (fn ([args] impl)\n" + "            ([args ranking-fn] impl))", "to")
			+ "))";
	/**
	 * Definition for convert-set clojure function
	 */
	public static String convertRepOrClojureDef = "(defn " + convertRepOrClojureSymbol + " [to arg]\n"
			+ "	(let [from (" + getTypeClojureSymbol + " arg)\n" + "		  reps (.getRepresentations from)]\n"
			+ "		(if (some identity \n" + // ~ (apply or...
			"				(map (fn [t] (try (velka.lang.types.Type/unifyTypes t to) (catch velka.lang.types.TypesDoesNotUnifyException e false)))\n"
			+ "					reps))\n"
			+ "			arg (throw (Throwable. (str \"Conversion from \" from \" to \" to \" does not exists.\"))))))";
	/**
	 * Definition for convert-to-set clojure function
	 */
	public static String convertToRepOrClojureDef = "(defn " + convertToRepOrClojureSymbol + " [to arg]\n"
			+ "	(let [from (" + getTypeClojureSymbol + " arg)\n" + "		  reps (.getRepresentations to)]\n"
			+ "		(if (some identity \n" + // ~ (apply or...
			"				(map (fn [t] (try (velka.lang.types.Type/unifyTypes t from) (catch velka.lang.types.TypesDoesNotUnifyException e false)))\n"
			+ "					reps))\n"
			+ "			arg (throw (Throwable. (str \"Conversion from \" from \" to \" to \" does not exists.\"))))))";
	/**
	 * Definition for convert clojure function
	 */
	public static String convertClojureDef = "(defn " + convertClojureSymbol + " [to arg]\n" + "    (let [from ("
			+ getTypeClojureSymbol + " arg)]" + "        (if (instance? velka.lang.types.TypeVariable to)\n"
			+ "            arg\n" + "            (if (instance? velka.lang.types.RepresentationOr to)\n"
			+ "				(" + convertToRepOrClojureSymbol + " to arg)\n"
			+ "            	(cond (instance? velka.lang.types.TypeAtom from) (" + convertAtomClojureSymbol
			+ " to arg)\n" + "                 	  (instance? velka.lang.types.TypeTuple from) ("
			+ convertTupleClojureSymbol + " to arg)\n"
			+ "                  	  (instance? velka.lang.types.TypeArrow from) (" + convertFnClojureSymbol
			+ " to arg)\n" + "                  	  (instance? velka.lang.types.RepresentationOr from) ("
			+ convertRepOrClojureSymbol + " to arg))))))";
	/**
	 * Definition for eapply function
	 */
	public static String eapplyClojureDef = "(defn " + eapplyClojureSymbol + "\n" + "    ([abstraction args ranking]\n"
			+ "        (let [impl (abstraction args ranking)\n" + "              converted-args ("
			+ convertClojureSymbol + "\n" + "                                 (.ltype (" + getTypeClojureSymbol
			+ " impl))\n" + "                                 args)]\n" + "            (apply impl converted-args)))\n"
			+ "    ([abstraction args]\n" + "        (let [impl (abstraction args)\n" + "              converted-args ("
			+ convertClojureSymbol + "\n" + "                                 (.ltype (" + getTypeClojureSymbol
			+ " impl))\n" + "                                 args)]\n" + "            (apply impl converted-args))))";
	/**
	 * Definition for select-implementation-clojure function
	 */
	public static String selectImplementationClojureDef = "(defn " + selectImplementationClojureSymbol + "\n"
			+ "    [ranking-fn args impls] \n" + "    (let [args-type (" + tuple2velkaListSymbol + "\n"
			+ "                        (map " + type2typeSymbolSymbol + " (" + getTypeClojureSymbol + " args)))]\n"
			+ "        (get \n" + "            (reduce \n"
			+ "                (fn [x y] (if (< (get x 0) (get y 0)) x y))\n" + "                (map \n"
			+ "                    (fn [impl] [\n" + "                        (first (" + eapplyClojureSymbol + "\n"
			+ "                                  ranking-fn\n"
			+ ClojureHelper.addTypeMetaInfo(
					"                                  [(" + tuple2velkaListSymbol + "\n"
							+ "                                       (map " + type2typeSymbolSymbol
							+ "                                                (.ltype (" + getTypeClojureSymbol
							+ " impl)))) \n" + "                                   args-type\n"
							+ "                                   args]",
					new TypeTuple(TypeAtom.TypeListNative, TypeAtom.TypeListNative))
			+ "))\n" + "                        impl])\n" + "                    impls))\n" + "        1)))";

	/**
	 * Creates record for atomic conversion map
	 * 
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
	 * Generates clojure code for definitions of velka.clojure.core namespace
	 * 
	 * @return string with code
	 */
	public static String writeDefinitions() {
		StringBuilder sb = new StringBuilder();
	
		sb.append(ClojureHelper.requireNamespace("clojure.string"));
		
		//Namespace
		sb.append(ClojureHelper.declareNamespace(VelkaClojureCore.NAMESPACE));
		
		// Declarations
		sb.append(ClojureHelper.makeDeclaration(type2typeSymbolSymbol));
		sb.append(ClojureHelper.makeDeclaration(tuple2velkaListSymbol));
		sb.append(ClojureHelper.makeDeclaration(getTypeClojureSymbol));
		sb.append(ClojureHelper.makeDynamicDeclaration(atomicConversionMapClojureSymbol));
		sb.append(ClojureHelper.makeDeclaration(convertAtomClojureSymbol));
		sb.append(ClojureHelper.makeDeclaration(convertTupleClojureSymbol));
		sb.append(ClojureHelper.makeDeclaration(convertFnClojureSymbol));
		sb.append(ClojureHelper.makeDeclaration(convertRepOrClojureSymbol));
		sb.append(ClojureHelper.makeDeclaration(convertToRepOrClojureSymbol));
		sb.append(ClojureHelper.makeDeclaration(convertClojureSymbol));
		sb.append(ClojureHelper.makeDeclaration(selectImplementationClojureSymbol));
		sb.append(ClojureHelper.makeDeclaration(eapplyClojureSymbol));
	
		// Definitions
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
	
		return sb.toString();
	}
	
	/**
	 * Writes file contents into given destination
	 * @param dest destination file path
	 * @return dest path
	 * @throws IOException 
	 */
	public static Path generateFile(Path dest) throws IOException {
		return Files.writeString(dest, VelkaClojureCore.writeDefinitions());
	}

	/**
	 * Relative path to velka.clojure.core file
	 */
	public static final Path VELKA_CLOJURE_CORE_PATH = Paths.get("velka", "clojure");
	
	/**
	 * Name of the velka.clojure.core file
	 */
	public static final Path VELKA_CLOJURE_CORE_NAME = Paths.get("core.clj");

	/**
	 * Relative path to file
	 */
	public static final Path RELATIVE_PATH = VELKA_CLOJURE_CORE_PATH.resolve(VELKA_CLOJURE_CORE_NAME);
	
}
