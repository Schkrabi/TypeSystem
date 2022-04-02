package velka.clojure;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

import velka.core.abstraction.ConversionOperators;
import velka.core.interpretation.ClojureCoreSymbols;
import velka.core.interpretation.ClojureHelper;
import velka.core.literal.LitComposite;
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
	
	private static final String getTypeClojure_expr = "_expr";
	/**
	 * Definition for get-type clojure symbol
	 */
	public static String getTypeClojureDef = 
			ClojureHelper.clojureDefnHelper(ClojureCoreSymbols.getTypeClojureSymbol, Arrays.asList(getTypeClojure_expr), 
					ClojureHelper.applyClojureFunction(":lang-type", 
							ClojureHelper.applyClojureFunction("meta", getTypeClojure_expr)));
	
	private static final String ListNativeToTuple_list = "_list";
	/**
	 * Definition of list-native-to-tuple function
	 */
	public static String listNativeToTuple =	
			ClojureHelper
					.clojureDefnHelper(ClojureCoreSymbols.listNativeToTuple, Arrays.asList(ListNativeToTuple_list),
							ClojureHelper.tupleHelper_str(ClojureHelper.getLiteralInnerValue(ListNativeToTuple_list)));
	
	
	private static final String tuple2velkaList_tuple = "_tuple";
	/**
	 * Definition for tuple-2-velka-list function
	 */
	public static String tuple2velkaListDef = 
			ClojureHelper.clojureDefnHelper(ClojureCoreSymbols.tuple2velkaListSymbol, 
					Arrays.asList(tuple2velkaList_tuple), 
					LitComposite.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction("lazy-seq", 
									ClojureHelper.applyClojureFunction("seq", tuple2velkaList_tuple)), 
							TypeAtom.TypeListNative));
	/**
	 * Definition for map of atomic type conversion
	 */
	public static String atomicConversionMapClojureDef = "(def ^:dynamic " + ClojureCoreSymbols.atomicConversionMapClojureSymbol + "{"
			+ ClojureHelper.makeAtomicConversionRecord(TypeAtom.TypeIntNative, TypeAtom.TypeIntString,
					ConversionOperators.IntNativeToIntString.clojureDef())
			+ "\n"
			+ ClojureHelper.makeAtomicConversionRecord(
					TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman, ConversionOperators.IntNativeToIntRoman.clojureDef())
			+ "\n"
			+ ClojureHelper.makeAtomicConversionRecord(TypeAtom.TypeIntString, TypeAtom.TypeIntNative,
					ConversionOperators.IntStringToIntNative.clojureDef())
			+ "\n"
			+ ClojureHelper.makeAtomicConversionRecord(
					TypeAtom.TypeIntString, TypeAtom.TypeIntRoman, ConversionOperators.IntStringToIntRoman.clojureDef())
			+ "\n"
			+ ClojureHelper.makeAtomicConversionRecord(TypeAtom.TypeIntRoman, TypeAtom.TypeIntNative,
					ConversionOperators.IntRomanToIntNative.clojureDef())
			+ "\n" + ClojureHelper.makeAtomicConversionRecord(TypeAtom.TypeIntRoman, TypeAtom.TypeIntString,
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
	
	private static String convertFn_to = "_to";
	private static String convertFn_arg = "_arg";
	private static String convertFn_from = "_from";
	private static String convertFn_impl = "_impl";
	private static String convertFn_fn_args = "_args";
	private static String convertFn_fn_ranking = "ranking";
	private static String convertFn_fn_arg = "_a";
	/**
	 * Definition for convert-fn clojure function
	 */
	public static String convertFnClojureDef = ClojureHelper.clojureDefnHelper(ClojureCoreSymbols.convertFnClojureSymbol, 
			Arrays.asList(convertFn_to, convertFn_arg), 
			ClojureHelper.letHelper(
						ClojureHelper.addTypeMetaInfo_str(
								ClojureHelper.fnHelper(
										new Pair<List<String>, String>(Arrays.asList(convertFn_fn_args), convertFn_impl),
										new Pair<List<String>, String>(Arrays.asList(convertFn_fn_args, convertFn_fn_ranking), convertFn_impl)), 
								convertFn_to), 
					new Pair<String, String>(convertFn_from, ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol, convertFn_arg)),
					new Pair<String, String>(convertFn_impl, 
							ClojureHelper.condHelper(
									new Pair<String, String>(
											ClojureHelper.applyClojureFunction("=", 
													ClojureHelper.applyClojureFunction(".ltype", convertFn_from),
													"velka.types.TypeTuple/EMPTY_TUPLE"),
											ClojureHelper.addTypeMetaInfo_str(
													ClojureHelper.fnHelper(Arrays.asList(), 
															ClojureHelper.applyClojureFunction(ClojureCoreSymbols.convertClojureSymbol, 
																	ClojureHelper.applyClojureFunction(".rtype", convertFn_to),
																	ClojureHelper.applyClojureFunction("apply",
																			ClojureHelper.applyClojureFunction(convertFn_arg, "nil"),
																			ClojureHelper.addTypeMetaInfo("[]", TypeTuple.EMPTY_TUPLE)))), 
													convertFn_to)),
									new Pair<String, String>(
											ClojureHelper.applyClojureFunction("instance?",
													"velka.types.TypeVariable",
													ClojureHelper.applyClojureFunction(".ltype", convertFn_to)),
											ClojureHelper.addTypeMetaInfo_str(
													ClojureHelper.fnHelper(
															Arrays.asList("& " + convertFn_fn_arg), 
															ClojureHelper.applyClojureFunction(ClojureCoreSymbols.convertClojureSymbol, 
																	ClojureHelper.applyClojureFunction(".rtype", convertFn_to),
																	ClojureHelper.applyClojureFunction("apply", 
																			ClojureHelper.applyClojureFunction(convertFn_arg, "nil"),
																			"_a"))), convertFn_to)),
									new Pair<String, String>(
											":else",
											ClojureHelper.addTypeMetaInfo_str(
													ClojureHelper.fnHelper(
															Arrays.asList("& " + convertFn_fn_arg), 
															ClojureHelper.applyClojureFunction(ClojureCoreSymbols.convertClojureSymbol, 
																	ClojureHelper.applyClojureFunction(".rtype", convertFn_to),
																	ClojureHelper.applyClojureFunction("apply", 
																			ClojureHelper.applyClojureFunction(convertFn_arg, "nil"),
																			ClojureHelper.applyClojureFunction(ClojureCoreSymbols.convertClojureSymbol,
																					ClojureHelper.applyClojureFunction(".ltype", convertFn_from),
																					ClojureHelper.addTypeMetaInfo_str(convertFn_fn_arg, 
																							ClojureHelper.applyClojureFunction(".ltype", convertFn_to)))))), 
													convertFn_to))))));
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
			+ ClojureCoreSymbols.convertClojureSymbol + "                                 (.ltype (" + ClojureCoreSymbols.getTypeClojureSymbol
			+ " impl))\n" + "                                 args)]\n" + "            (apply impl converted-args)))\n"
			+ "    ([abstraction args]\n" + "        (let [impl (abstraction args)\n" + "              converted-args ("
			+ ClojureCoreSymbols.convertClojureSymbol  + "                                 (.ltype (" + ClojureCoreSymbols.getTypeClojureSymbol
			+ " impl))\n" + "                                 args)]\n" + "            (apply impl converted-args))))";
	
	
	private static final String langPstrClojureDef_expr = "_expr";
	private static final String langPstrClojureDef_level = "_level";
	private static final String langPstrClojureDef_aux = "_aux";
	private static final String langPstrClojureDef_type = "_type";
	private static final String langPstrClojureDef_vec = "_vec";
	private static final String langPstrClojureDef_x = "_x";
	public static final String langPstrClojureDef = 
			ClojureHelper.clojureDefnHelper(ClojureCoreSymbols.langPstrClojure, 
					Arrays.asList(langPstrClojureDef_expr), 
					ClojureHelper.letfnHelper(
							ClojureHelper.applyClojureFunction(langPstrClojureDef_aux, langPstrClojureDef_expr, "0"), 
							ClojureHelper.makeLetfnTriplet(langPstrClojureDef_aux, 
									Arrays.asList(langPstrClojureDef_expr, langPstrClojureDef_level), 
									ClojureHelper.letHelper(
											ClojureHelper.condHelper(
													new Pair<String, String>(
															ClojureHelper.applyClojureFunction("or", 
																	ClojureHelper.applyClojureFunction("=", 
																			langPstrClojureDef_type, 
																			"velka.types.TypeAtom/TypeIntNative"),
																	ClojureHelper.applyClojureFunction("=", 
																			langPstrClojureDef_type, 
																			"velka.types.TypeAtom/TypeStringNative"),
																	ClojureHelper.applyClojureFunction("=", 
																			langPstrClojureDef_type, 
																			"velka.types.TypeAtom/TypeDoubleNative"),
																	ClojureHelper.applyClojureFunction("=", 
																			langPstrClojureDef_type, 
																			"velka.types.TypeAtom/TypeIntNative")), 
															ClojureHelper.clojureIfHelper(
																	ClojureHelper.applyClojureFunction("=", langPstrClojureDef_level, "0"), 
																	ClojureHelper.applyClojureFunction("pr-str", ClojureHelper.getLiteralInnerValue(langPstrClojureDef_expr)), 
																	ClojureHelper.getLiteralInnerValue(langPstrClojureDef_expr))),
													new Pair<String, String>(
															ClojureHelper.applyClojureFunction("=", 
																	langPstrClojureDef_type, "velka.types.TypeTuple/EMPTY_TUPLE"), 
															"[]"),
													new Pair<String, String>(
															ClojureHelper.applyClojureFunction("or",
																ClojureHelper.applyClojureFunction("instance?", 
																		langPstrClojureDef_type,
																		TypeTuple.class.getCanonicalName()),
																ClojureHelper.applyClojureFunction("=", 
																		langPstrClojureDef_type,
																		"velka.types.TypeAtom/TypeListNative")), 
															ClojureHelper.letHelper(
																ClojureHelper.clojureIfHelper(
																		ClojureHelper.applyClojureFunction("=", langPstrClojureDef_level, "0"), 
																		ClojureHelper.applyClojureFunction("pr-str", langPstrClojureDef_vec), 
																		langPstrClojureDef_vec),
																new Pair<String, String>(langPstrClojureDef_vec, 
																		ClojureHelper.applyClojureFunction("vec", 
																			ClojureHelper.applyClojureFunction("map", 
																					ClojureHelper.fnHelper(Arrays.asList(langPstrClojureDef_x), 
																							ClojureHelper.applyClojureFunction(langPstrClojureDef_aux, 
																									langPstrClojureDef_x,
																									ClojureHelper.applyClojureFunction("+", langPstrClojureDef_level, "1"))),
																					langPstrClojureDef_expr))))),
													new Pair<String, String>(
															ClojureHelper.applyClojureFunction("instance?", 
																	langPstrClojureDef_type,
																	TypeAtom.class.getCanonicalName()), 
															ClojureHelper.applyClojureFunction(langPstrClojureDef_aux, 
																	ClojureHelper.getLiteralInnerValue(langPstrClojureDef_expr),
																	langPstrClojureDef_level)),
													new Pair<String, String>(
															":else",
															ClojureHelper.errorHelper(ClojureHelper.applyClojureFunction("str", 
																	langPstrClojureDef_expr,
																	ClojureHelper.stringHelper("is not a printable expression"))))), 
											new Pair<String, String>(langPstrClojureDef_type, 
													ClojureHelper.applyClojureFunction(
															ClojureCoreSymbols.getTypeClojureSymbol_full, langPstrClojureDef_expr))))));

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
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.listNativeToTuple));
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
		sb.append(listNativeToTuple);
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
		sb.append(eapplyClojureDef);
		sb.append("\n");
//		sb.append("(def lang-pstr" + "(fn [exp]" + "(letfn [(lang-pstr-aux [exp level]"
//				+ "(let [type (:lang-type (meta exp))]" + "(cond" + "(or"
//				+ "(= type velka.types.TypeAtom/TypeIntNative)"
//				+ "(= type velka.types.TypeAtom/TypeStringNative)"
//				+ "(= type velka.types.TypeAtom/TypeDoubleNative)"
//				+ "(= type velka.types.TypeAtom/TypeBoolNative))" + "(if (= level 0)" + "(pr-str (get exp 0))"
//				+ "(get exp 0))" + "(= type velka.types.TypeTuple/EMPTY_TUPLE) []"
//				+ "(instance? velka.types.TypeAtom type) (lang-pstr-aux (get exp 0) level)"
//				+ "(instance? velka.types.TypeTuple type) " + "(if" + "(= level 0)"
//				+ "(pr-str (vec (map (fn [x] (lang-pstr-aux x (+ level 1))) exp)))"
//				+ "(vec (map (fn [x] (lang-pstr-aux x (+ level 1))) exp)))"
//				+ ":else (throw (Throwable. (str exp \" is not a printable expression\"))))))]"
//				+ "(lang-pstr-aux exp 0))))");
		sb.append(langPstrClojureDef);
	
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
