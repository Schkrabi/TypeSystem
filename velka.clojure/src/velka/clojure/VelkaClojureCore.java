package velka.clojure;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;

import velka.core.abstraction.ConversionOperators;
import velka.core.interpretation.ClojureCoreSymbols;
import velka.core.interpretation.ClojureHelper;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.util.Pair;

/**
 * This class is generating velka.clojure.core namespace file velka/clojure/core.clj
 * @author Mgr. Radomir Skrabal
 *
 */
public class VelkaClojureCore {

	/**
	 * Definition for type-2-type-symbol function
	 */
	public static String type2typeSymbolDef = "(defn " + ClojureCoreSymbols.type2typeSymbolSymbol + " [type] \n"
			+ ClojureHelper.addTypeMetaInfo_str("[type]", "type") + ")";
	/**
	 * Definition for get-type clojure symbol
	 */
	public static String getTypeClojureDef = "(defn " + ClojureCoreSymbols.getTypeClojureSymbol + " [expr] (:lang-type (meta expr)))";
	/**
	 * Definition for tuple-2-velka-list function
	 */
	public static String tuple2velkaListDef = "(defn " + ClojureCoreSymbols.tuple2velkaListSymbol + " [tuple] \n" + "    (reduce \n"
			+ "        (fn [rest x] "
			+ ClojureHelper.addTypeMetaInfo(
					"[" + ClojureHelper.addTypeMetaInfo_str("[x rest]",
							"(velka.types.TypeTuple. [(" + ClojureCoreSymbols.getTypeClojureSymbol + " x) "
									+ TypeAtom.TypeListNative.clojureTypeRepresentation() + "])")
							+ "]",
					TypeAtom.TypeListNative)
			+ ") \n" + "        " + ClojureHelper
					.addTypeMetaInfo("[" + ClojureHelper.addTypeMetaInfo("[]", TypeTuple.EMPTY_TUPLE) + "]", TypeAtom.TypeListNative)
			+ " (reverse tuple)))";
	/**
	 * Definition for map of atomic type conversion
	 */
	public static String atomicConversionMapClojureDef = "(def ^:dynamic " + ClojureCoreSymbols.atomicConversionMapClojureSymbol + "{"
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
	public static String convertAtomClojureDef = "(defn " + ClojureCoreSymbols.convertAtomClojureSymbol + " [to arg]\n"
			+ "    (let [from (" + ClojureCoreSymbols.getTypeClojureSymbol + " arg)]\n" + "        (cond (= from to) arg\n"
			+ "              (and (instance? velka.types.TypeAtom to) (= (.representation to) velka.types.TypeRepresentation/WILDCARD)) arg\n"
			+ "              (contains? " + ClojureCoreSymbols.atomicConversionMapClojureSymbol + " [from to])\n"
			+ "                  (((get " + ClojureCoreSymbols.atomicConversionMapClojureSymbol + " [from to]) nil) arg)\n"
			+ "              :else (throw (Throwable. (str \"Conversion from \" from \" to \" to \" does not exists.\"))))))";
	/**
	 * Definition for convert-tuple clojure function
	 */
	public static String convertTupleClojureDef = "(defn " + ClojureCoreSymbols.convertTupleClojureSymbol + " [to arg]\n"
			+ ClojureHelper.addTypeMetaInfo_str("    (vec (map " + ClojureCoreSymbols.convertClojureSymbol + " to arg))", "to") + ")";
	/**
	 * Definition for convert-fn clojure function
	 */
	public static String convertFnClojureDef = "(defn " + ClojureCoreSymbols.convertFnClojureSymbol + " [to arg]\n" + "    (let [from ("
			+ ClojureCoreSymbols.getTypeClojureSymbol + " arg)\n" + "          impl "
			+ ClojureHelper.addTypeMetaInfo_str("(fn [& a] (" + ClojureCoreSymbols.convertClojureSymbol + " (.rtype to)\n"
					+ "                                (apply (arg nil) " + "										("
					+ ClojureCoreSymbols.convertClojureSymbol + " 											(.ltype from) "
					+ ClojureHelper.addTypeMetaInfo_str("a", "(.ltype to)") + "))))", "to")
			+ "]\n" + ClojureHelper.addTypeMetaInfo_str("        (fn ([args] impl)\n" + "            ([args ranking-fn] impl))", "to")
			+ "))";
	/**
	 * Definition for convert-set clojure function
	 */
	public static String convertRepOrClojureDef = "(defn " + ClojureCoreSymbols.convertRepOrClojureSymbol + " [to arg]\n"
			+ "	(let [from (" + ClojureCoreSymbols.getTypeClojureSymbol + " arg)\n" + "		  reps (.getRepresentations from)]\n"
			+ "		(if (some identity \n" + // ~ (apply or...
			"				(map (fn [t] (try (velka.types.Type/unifyTypes t to) (catch velka.types.TypesDoesNotUnifyException e false)))\n"
			+ "					reps))\n"
			+ "			arg (throw (Throwable. (str \"Conversion from \" from \" to \" to \" does not exists.\"))))))";
	/**
	 * Definition for convert-to-set clojure function
	 */
	public static String convertToRepOrClojureDef = "(defn " + ClojureCoreSymbols.convertToRepOrClojureSymbol + " [to arg]\n"
			+ "	(let [from (" + ClojureCoreSymbols.getTypeClojureSymbol + " arg)\n" + "		  reps (.getRepresentations to)]\n"
			+ "		(if (some identity \n" + // ~ (apply or...
			"				(map (fn [t] (try (velka.types.Type/unifyTypes t from) (catch velka.types.TypesDoesNotUnifyException e false)))\n"
			+ "					reps))\n"
			+ "			arg (throw (Throwable. (str \"Conversion from \" from \" to \" to \" does not exists.\"))))))";
	/**
	 * Definition for convert clojure function
	 */
	public static String convertClojureDef = "(defn " + ClojureCoreSymbols.convertClojureSymbol + " [to arg]\n" + "    (let [from ("
			+ ClojureCoreSymbols.getTypeClojureSymbol + " arg)]" + "        (if (instance? velka.types.TypeVariable to)\n"
			+ "            arg\n" + "            (if (instance? velka.types.RepresentationOr to)\n"
			+ "				(" + ClojureCoreSymbols.convertToRepOrClojureSymbol + " to arg)\n"
			+ "            	(cond (instance? velka.types.TypeAtom from) (" + ClojureCoreSymbols.convertAtomClojureSymbol
			+ " to arg)\n" + "                 	  (instance? velka.types.TypeTuple from) ("
			+ ClojureCoreSymbols.convertTupleClojureSymbol + " to arg)\n"
			+ "                  	  (instance? velka.types.TypeArrow from) (" + ClojureCoreSymbols.convertFnClojureSymbol
			+ " to arg)\n" + "                  	  (instance? velka.types.RepresentationOr from) ("
			+ ClojureCoreSymbols.convertRepOrClojureSymbol + " to arg))))))";
	/**
	 * Definition for eapply function
	 */
	public static String eapplyClojureDef = "(defn " + ClojureCoreSymbols.eapplyClojureSymbol + "\n" + "    ([abstraction args ranking]\n"
			+ "        (let [impl (abstraction args ranking)\n" + "              converted-args ("
			+ ClojureCoreSymbols.convertClojureSymbol + "\n" + "                                 (.ltype (" + ClojureCoreSymbols.getTypeClojureSymbol
			+ " impl))\n" + "                                 args)]\n" + "            (apply impl converted-args)))\n"
			+ "    ([abstraction args]\n" + "        (let [impl (abstraction args)\n" + "              converted-args ("
			+ ClojureCoreSymbols.convertClojureSymbol + "\n" + "                                 (.ltype (" + ClojureCoreSymbols.getTypeClojureSymbol
			+ " impl))\n" + "                                 args)]\n" + "            (apply impl converted-args))))";
	
	private static String selectImplRankingFn = "_ranking-fn";
	private static String selectImplArgs = "_args";
	private static String selectImplImpls = "_impls";
	private static String selectImplArgsType = "_args-type";
	private static String selectImplArgsList = "_args-list";
	private static String impl = "_impl";
	private static String x = "_x";
	private static String y = "_y";
	
	/**
	 * Definition for select-implementation-clojure function
	 */
	public static String selectImplementationClojureDef = ClojureHelper
			.clojureDefnHelper(ClojureCoreSymbols.selectImplementationClojureSymbol,
					Arrays.asList(selectImplRankingFn, selectImplArgs, selectImplImpls),
					ClojureHelper.letHelper(
							"(second (reduce "
									+ ClojureHelper.fnHelper(
											Arrays.asList(x, y), "(if (< (first " + x + ") (first " + y + ")) " + x + " " + y + ")")
									+ " (map "
									+ ClojureHelper.fnHelper(
											Arrays.asList(impl), 
											ClojureHelper.letHelper(
												ClojureHelper.clojureVectorHelper(
														"(first " + ClojureHelper.applyVelkaFunction(selectImplRankingFn, selectImplArgsType, selectImplArgsList) + ")", 
														impl),
												new Pair<String, String>(selectImplArgsType,
														"(" + ClojureCoreSymbols.tuple2velkaListSymbol + "\n" + "(map "
																+ ClojureCoreSymbols.type2typeSymbolSymbol + "(.ltype ("
																+ ClojureCoreSymbols.getTypeClojureSymbol + " " + impl + "))))")))
									+ " " + selectImplImpls + ")))",
							new Pair<String, String>(selectImplArgsList,
									"(" + ClojureCoreSymbols.tuple2velkaListSymbol + " " + selectImplArgs + ")")));
	
//	/**
//	 * Definition for select-implementation-clojure function
//	 */
//	public static String selectImplementationClojureDef = 
//			  "(defn " + ClojureCoreSymbols.selectImplementationClojureSymbol + "\n"
//			+ "    [ranking-fn args impls] \n" 
//		    + "    (let [args-type (" + ClojureCoreSymbols.tuple2velkaListSymbol + " args)]\n"
//			+ "        (get \n" 
//			+ "            (reduce \n"
//			+ "                (fn [x y] (if (< (get x 0) (get y 0)) x y))\n" 
//			+ "                (map \n"
//			+ "                    (fn [impl] [\n" 
//			+ "                        (first (" + ClojureCoreSymbols.eapplyClojureSymbol + "\n"
//			+ "                                  ranking-fn\n" + ClojureHelper.addTypeMetaInfo(
//	          "                                  [(" + ClojureCoreSymbols.tuple2velkaListSymbol + "\n"
//			+ "                                       (map " + ClojureCoreSymbols.type2typeSymbolSymbol
//			+ "                                                (.ltype (" + ClojureCoreSymbols.getTypeClojureSymbol + " impl)))) \n" 
//			+ "                                   args-type\n"
//			+ "                                   args]", new TypeTuple(TypeAtom.TypeListNative, TypeAtom.TypeListNative)) + "))\n" 
//			+ "                        impl])\n" 
//			+ "                    impls))\n" 
//			+ "        1)))";

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
		sb.append(ClojureHelper.declareNamespace(ClojureCoreSymbols.NAMESPACE));
		
		// Declarations
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.type2typeSymbolSymbol));
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.tuple2velkaListSymbol));
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.getTypeClojureSymbol));
		sb.append(ClojureHelper.makeDynamicDeclaration(ClojureCoreSymbols.atomicConversionMapClojureSymbol));
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.convertAtomClojureSymbol));
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.convertTupleClojureSymbol));
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.convertFnClojureSymbol));
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.convertRepOrClojureSymbol));
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.convertToRepOrClojureSymbol));
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.convertClojureSymbol));
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.selectImplementationClojureSymbol));
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.eapplyClojureSymbol));
	
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
				+ "(= type velka.types.TypeAtom/TypeIntNative)"
				+ "(= type velka.types.TypeAtom/TypeStringNative)"
				+ "(= type velka.types.TypeAtom/TypeDoubleNative)"
				+ "(= type velka.types.TypeAtom/TypeBoolNative))" + "(if (= level 0)" + "(pr-str (get exp 0))"
				+ "(get exp 0))" + "(= type velka.types.TypeTuple/EMPTY_TUPLE) []"
				+ "(instance? velka.types.TypeAtom type) (lang-pstr-aux (get exp 0) level)"
				+ "(instance? velka.types.TypeTuple type) " + "(if" + "(= level 0)"
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
