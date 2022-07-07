package velka.clojure;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;

import velka.core.abstraction.ConversionOperators;
import velka.core.literal.LitComposite;
import velka.types.RepresentationOr;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.types.TypesDoesNotUnifyException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.Pair;

/**
 * This class is generating velka.clojure.core namespace file velka/clojure/core.clj
 * @author Mgr. Radomir Skrabal
 *
 */
public class VelkaClojureCore {
	
	private static final String type2typeSymbol_type = "_type";
	/**
	 * Definition for type-2-type-symbol function
	 */
	public static String type2typeSymbolDef = ClojureHelper.clojureDefnHelper(
			ClojureCoreSymbols.type2typeSymbolSymbol,
			Arrays.asList(type2typeSymbol_type),
			ClojureHelper.addTypeMetaInfo_str(
					ClojureHelper.clojureVectorHelper(type2typeSymbol_type),
					type2typeSymbol_type)); 
	
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
	public static String atomicConversionMapClojureDef = ClojureHelper.dynamicDef(
			ClojureCoreSymbols.atomicConversionMapClojureSymbol,
			ClojureHelper.mapHelper(
					new Pair<String, String>(
							ClojureHelper.clojureVectorHelper(
									TypeAtom.TypeIntNative.clojureTypeRepresentation(),
									TypeAtom.TypeIntString.clojureTypeRepresentation()),
							ConversionOperators.IntNativeToIntString.clojureDef()),
					new Pair<String, String>(
							ClojureHelper.clojureVectorHelper(
									TypeAtom.TypeIntNative.clojureTypeRepresentation(),
									TypeAtom.TypeIntRoman.clojureTypeRepresentation()),
							ConversionOperators.IntNativeToIntRoman.clojureDef()),
					new Pair<String, String>(
							ClojureHelper.clojureVectorHelper(
									TypeAtom.TypeIntString.clojureTypeRepresentation(),
									TypeAtom.TypeIntNative.clojureTypeRepresentation()),
							ConversionOperators.IntStringToIntNative.clojureDef()),
					new Pair<String, String>(
							ClojureHelper.clojureVectorHelper(
									TypeAtom.TypeIntString.clojureTypeRepresentation(),
									TypeAtom.TypeIntRoman.clojureTypeRepresentation()),
							ConversionOperators.IntStringToIntRoman.clojureDef()),
					new Pair<String, String>(
							ClojureHelper.clojureVectorHelper(
									TypeAtom.TypeIntRoman.clojureTypeRepresentation(),
									TypeAtom.TypeIntNative.clojureTypeRepresentation()),
							ConversionOperators.IntRomanToIntNative.clojureDef()),
					new Pair<String, String>(
							ClojureHelper.clojureVectorHelper(
									TypeAtom.TypeIntRoman.clojureTypeRepresentation(),
									TypeAtom.TypeIntString.clojureTypeRepresentation()),
							ConversionOperators.IntRomanToIntString.clojureDef())));
	
	private static String convert_to = "_to";
	private static String convert_expr = "_expr";
	private static String convert_from = "_from";
	/**
	 * Definition for convert-type-atom clojure function
	 */
	public static String convertAtomClojureDef = ClojureHelper.clojureDefnHelper(
			ClojureCoreSymbols.convertAtomClojureSymbol,
			Arrays.asList(convert_to, convert_expr),
			ClojureHelper.letHelper(
					ClojureHelper.condHelper(
							new Pair<String, String>(
									ClojureHelper.applyClojureFunction(
											"=",
											convert_from,
											convert_to),
									convert_expr),
							new Pair<String, String>(
									ClojureHelper.applyClojureFunction(
											"and",
											ClojureHelper.isInstanceOfClass(
													convert_to,
													TypeAtom.class),
											ClojureHelper.applyClojureFunction(
													"=",
													ClojureHelper.applyClojureFunction(
															".representation",
															convert_to),
													"velka.types.TypeRepresentation/WILDCARD")
													),
											convert_expr),
							new Pair <String, String>(
									ClojureHelper.applyClojureFunction(
											"contains?",
											ClojureCoreSymbols.atomicConversionMapClojureSymbol,
											ClojureHelper.clojureVectorHelper(
													convert_from,
													convert_to)),
									ClojureHelper.applyVelkaFunction(
										ClojureHelper.applyClojureFunction(
												"get",
												ClojureCoreSymbols.atomicConversionMapClojureSymbol,
												ClojureHelper.clojureVectorHelper(
														convert_from,
														convert_to)),
										convert_expr)),
							new Pair<String, String>(
									":else",
									ClojureHelper.errorHelper(
											ClojureHelper.applyClojureFunction(
													"str",
													ClojureHelper.stringHelper("Type atom conversion from "),
													convert_from,
													ClojureHelper.stringHelper(" to "),
													convert_to,
													ClojureHelper.stringHelper(" does not exist, when converting "),
													convert_expr)))),
					new Pair<String, String>(
							convert_from, 
							ClojureHelper.applyClojureFunction(
									ClojureCoreSymbols.getTypeClojureSymbol, 
									convert_expr))));

	/**
	 * Definition for convert-tuple clojure function
	 */
	public static String convertTupleClojureDef = ClojureHelper.clojureDefnHelper(
			ClojureCoreSymbols.convertTupleClojureSymbol,
			Arrays.asList(convert_to, convert_expr),
			ClojureHelper.addTypeMetaInfo_str(
					ClojureHelper.applyClojureFunction(
							"vec",
							ClojureHelper.applyClojureFunction(
									"map",
									ClojureCoreSymbols.convertClojureSymbol,
									convert_to,
									convert_expr)),
					convert_to)); 

	private static String convertFn_to = "_to";
	private static String convertFn_toNormal = "_toNormal";
	private static String convertFn_expr = "_expr";
	private static String convertFn_from = "_from";
	private static String convertFn_impl = "_impl";
	private static String convertFn_fn_arg = "_a";
	/**
	 * Definition for convert-fn clojure function
	 */
	public static String convertFnClojureDef = ClojureHelper.clojureDefnHelper(ClojureCoreSymbols.convertFnClojureSymbol, 
			Arrays.asList(convertFn_to, convertFn_expr), 
			ClojureHelper.letHelper(
					convertFn_impl,
					new Pair<String, String>(
							convertFn_from, 
							ClojureHelper.applyClojureFunction(
									ClojureCoreSymbols.getTypeClojureSymbol, 
									convertFn_expr)),
					new Pair<String, String>(
						convertFn_toNormal, 
						ClojureHelper.applyClojureFunction(
							"velka.types.TypeArrow.",
							ClojureHelper.applyClojureFunction(
									"if",
									ClojureHelper.applyClojureFunction(
											"instance?",
											"velka.types.TypeTuple",
											ClojureHelper.applyClojureFunction(
													".ltype",
													convertFn_to)),
									ClojureHelper.applyClojureFunction(
											".ltype", 
											convertFn_to),
									ClojureHelper.applyClojureFunction(
											".ltype",
											convertFn_from)),
							ClojureHelper.applyClojureFunction(
								".rtype",
								convertFn_to))),
					new Pair<String, String>(convertFn_impl, 
							ClojureHelper.condHelper(
									new Pair<String, String>(
											ClojureHelper.applyClojureFunction("=", 
													ClojureHelper.applyClojureFunction(".ltype", convertFn_from),
													"velka.types.TypeTuple/EMPTY_TUPLE"),
											ClojureHelper.addTypeMetaInfo_str(
													ClojureHelper.fnHelper(Arrays.asList(), 
															ClojureHelper.applyClojureFunction(ClojureCoreSymbols.convertClojureSymbol, 
																	ClojureHelper.applyClojureFunction(".rtype", convertFn_toNormal),
																	ClojureHelper.applyVelkaFunction_argsTuple(
																			convertFn_expr,
																			Type.addTypeMetaInfo("[]", TypeTuple.EMPTY_TUPLE)))), 
													convertFn_toNormal)),
									new Pair<String, String>(
											ClojureHelper.applyClojureFunction("instance?",
													"velka.types.TypeVariable",
													ClojureHelper.applyClojureFunction(".ltype", convertFn_toNormal)),
											ClojureHelper.addTypeMetaInfo_str(
													ClojureHelper.fnHelper(
															Arrays.asList("& " + convertFn_fn_arg), 
															ClojureHelper.applyClojureFunction(ClojureCoreSymbols.convertClojureSymbol, 
																	ClojureHelper.applyClojureFunction(".rtype", convertFn_toNormal),
																	ClojureHelper.applyVelkaFunction_argsTuple( 
																			convertFn_expr,
																			"_a"))), convertFn_toNormal)),
									new Pair<String, String>(
											":else",
											ClojureHelper.addTypeMetaInfo_str(
													ClojureHelper.fnHelper(
															Arrays.asList("& " + convertFn_fn_arg), 
															ClojureHelper.applyClojureFunction(ClojureCoreSymbols.convertClojureSymbol, 
																	ClojureHelper.applyClojureFunction(".rtype", convertFn_toNormal),
																	ClojureHelper.applyVelkaFunction_argsTuple( 
																			convertFn_expr,
																			ClojureHelper.applyClojureFunction(ClojureCoreSymbols.convertClojureSymbol,
																					ClojureHelper.applyClojureFunction(".ltype", convertFn_from),
																					ClojureHelper.addTypeMetaInfo_str(convertFn_fn_arg, 
																							ClojureHelper.applyClojureFunction(".ltype", convertFn_toNormal)))))), 
													convertFn_toNormal))))));
	
	private static String convert_reps = "_reps";
	private static String convert_t = "_t";
	private static String convert_e = "_e";
	/**
	 * Definition for convert-set clojure function
	 */
	public static String convertRepOrClojureDef = ClojureHelper.clojureDefnHelper(
			ClojureCoreSymbols.convertRepOrClojureSymbol,
			Arrays.asList(convert_to, convert_expr),
			ClojureHelper.letHelper(
					ClojureHelper.applyClojureFunction(
							"if",
							ClojureHelper.applyClojureFunction(
									"some",
									"identity",
									ClojureHelper.applyClojureFunction(
											"map",
											ClojureHelper.fnHelper(
													Arrays.asList(convert_t),
													ClojureHelper.tryCatchHelper(
															ClojureHelper.applyClojureFunction(
																	"velka.types.Type/unifyTypes",
																	convert_t,
																	convert_to),
															TypesDoesNotUnifyException.class,
															convert_e,
															"false")),
											convert_reps)),
							convert_expr,
							ClojureHelper.errorHelper(
									ClojureHelper.applyClojureFunction(
											"str",
											ClojureHelper.stringHelper("Cannot convert "),
											convert_from,
											ClojureHelper.stringHelper(" to "),
											convert_to,
											ClojureHelper.stringHelper(" converted value "),
											convert_expr))),
					new Pair<String, String>(
							convert_from,
							ClojureHelper.applyClojureFunction(
									ClojureCoreSymbols.getTypeClojureSymbol,
									convert_expr)),
					new Pair<String, String>(
							convert_reps,
							ClojureHelper.applyClojureFunction(
									".getRepresentations",
									convert_from)))); 
			
	/**
	 * Definition for convert-to-set clojure function
	 */
	public static String convertToRepOrClojureDef = ClojureHelper.clojureDefnHelper(
			ClojureCoreSymbols.convertToRepOrClojureSymbol,
			Arrays.asList(convert_to, convert_expr),
			ClojureHelper.letHelper(
					ClojureHelper.applyClojureFunction(
							"if",
							ClojureHelper.applyClojureFunction(
									"some",
									"identity",
									ClojureHelper.applyClojureFunction(
											"map",
											ClojureHelper.fnHelper(
													Arrays.asList(convert_t),
													ClojureHelper.tryCatchHelper(
															ClojureHelper.applyClojureFunction(
																	"velka.types.Type/unifyTypes",
																	convert_t,
																	convert_from),
															TypesDoesNotUnifyException.class,
															convert_e,
															"false")),
											convert_reps)),
							convert_expr,
							ClojureHelper.errorHelper(
									ClojureHelper.applyClojureFunction(
											"str",
											ClojureHelper.stringHelper("Cannot convert "),
											convert_from,
											ClojureHelper.stringHelper(" to "),
											convert_to,
											ClojureHelper.stringHelper(" converted value "),
											convert_expr))),
					new Pair<String, String>(
							convert_from,
							ClojureHelper.applyClojureFunction(
									ClojureCoreSymbols.getTypeClojureSymbol,
									convert_expr)),
					new Pair<String, String>(
							convert_reps,
							ClojureHelper.applyClojureFunction(
									".getRepresentations",
									convert_to))));
			
	/**
	 * Definition for convert clojure function
	 */
	public static String convertClojureDef = 
			ClojureHelper.clojureDefnHelper(
					ClojureCoreSymbols.convertClojureSymbol,
					Arrays.asList(convert_to, convert_expr),
					ClojureHelper.letHelper(
							ClojureHelper.condHelper(
									new Pair<String, String>(
											ClojureHelper.applyClojureFunction(
													"or",
													ClojureHelper.isInstanceOfClass(
															convert_from,
															TypeVariable.class),
													ClojureHelper.isInstanceOfClass(
															convert_to,
															TypeVariable.class)),
											convert_expr),
									new Pair<String, String>(
											ClojureHelper.isInstanceOfClass(
													convert_to,
													RepresentationOr.class),
											ClojureHelper.applyClojureFunction(
													ClojureCoreSymbols.convertToRepOrClojureSymbol,
													convert_to,
													convert_expr)),
									new Pair<String, String>(
											ClojureHelper.isInstanceOfClass(
													convert_from,
													RepresentationOr.class),
											ClojureHelper.applyClojureFunction(
													ClojureCoreSymbols.convertRepOrClojureSymbol,
													convert_to,
													convert_expr)),
									new Pair<String, String>(
											ClojureHelper.isInstanceOfClass(
													convert_from,
													TypeAtom.class),
											ClojureHelper.applyClojureFunction(
													ClojureCoreSymbols.convertAtomClojureSymbol,
													convert_to,
													convert_expr)),
									new Pair<String, String>(
											ClojureHelper.isInstanceOfClass(
													convert_from,
													TypeTuple.class),
											ClojureHelper.applyClojureFunction(
													ClojureCoreSymbols.convertTupleClojureSymbol,
													convert_to,
													convert_expr)),
									new Pair<String, String>(
											ClojureHelper.isInstanceOfClass(
													convert_from,
													TypeArrow.class),
											ClojureHelper.applyClojureFunction(
													ClojureCoreSymbols.convertFnClojureSymbol,
													convert_to,
													convert_expr))),
							new Pair<String, String>(
									convert_from,
									ClojureHelper.applyClojureFunction(
											ClojureCoreSymbols.getTypeClojureSymbol,
											convert_expr))));
	
	private static String eapply_abstr = "_abstr";
	private static String eapply_args = "_args";
	private static String eapply_costF = "_costF";
	private static String eapply_impl = "_impl";
	private static String eapply_fn_i = "_i";
	private static String eapply_fn_p1 = "_p1";
	private static String eapply_fn_p2 = "_p2";
	/**
	 * Definition for eapply function
	 */
	public static String eapplyClojureDef = ClojureHelper.clojureDefnHelper(
			ClojureCoreSymbols.eapplyClojureSymbol,
			Arrays.asList(eapply_abstr, eapply_args, eapply_costF),
			ClojureHelper.letHelper(
					ClojureHelper.applyClojureFunction(
							"apply",
							eapply_impl,
							ClojureHelper.applyClojureFunction(
									ClojureCoreSymbols.convertClojureSymbol,
									ClojureHelper.applyClojureFunction(
											".ltype",
											ClojureHelper.applyClojureFunction(
													ClojureCoreSymbols.getTypeClojureSymbol,
													eapply_impl)),
									eapply_args)),
					new Pair<String, String>(
							eapply_impl,
							ClojureHelper.condHelper(
									new Pair<String, String>(
											ClojureHelper.applyClojureFunction("fn?", eapply_abstr),
											eapply_abstr),
									new Pair<String, String>(
											ClojureHelper.applyClojureFunction("set?", eapply_abstr),
											ClojureHelper.applyClojureFunction(
													"first",
													ClojureHelper.applyClojureFunction(
															"reduce",
															ClojureHelper.fnHelper(
																	Arrays.asList(eapply_fn_p1, eapply_fn_p2),
																	ClojureHelper.applyClojureFunction(
																			"if",
																			ClojureHelper.applyClojureFunction(
																					"<",
																					ClojureHelper.applyClojureFunction(
																							"second",
																							eapply_fn_p1),
																					ClojureHelper.applyClojureFunction(
																							"second",
																							eapply_fn_p2)),
																			eapply_fn_p1,
																			eapply_fn_p2)),
															ClojureHelper.applyClojureFunction(
																	"map",
																	ClojureHelper.fnHelper(
																			Arrays.asList(eapply_fn_i),
																			ClojureHelper.clojureVectorHelper(
																					eapply_fn_i,
																					ClojureHelper.getLiteralInnerValue(
																						ClojureHelper.applyVelkaFunction_argsTuple(
																								ClojureHelper.applyClojureFunction(
																										"if",
																										ClojureHelper.applyClojureFunction(
																												"nil?",
																												eapply_costF),
																										ClojureHelper.applyClojureFunction(
																												ClojureCoreSymbols.getCostFunction_full,
																												eapply_fn_i),
																										eapply_costF),
																								eapply_args)))),
																	eapply_abstr)))),
									new Pair<String, String>(
											":else",
											ClojureHelper.errorHelper(
													ClojureHelper.applyClojureFunction(
															"str",
															ClojureHelper.stringHelper("invalid abstraction "),
															eapply_abstr,
															ClojureHelper.stringHelper(" in (eapply "),
															eapply_abstr,
															ClojureHelper.stringHelper(" "),
															eapply_args,
															ClojureHelper.stringHelper(")"))))))));
	
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
	
	private static final String asFunctionClojureDef_f = "_f";
	private static final String asFunctionClojureDef_this = "_this";
	private static final String asFunctionClojureDef_arg = "_arg";
	public static final String asFunctionClojureDef = 
			ClojureHelper.clojureDefnHelper(ClojureCoreSymbols.asFunctionClojure,
					Arrays.asList(asFunctionClojureDef_f),
					ClojureHelper.applyClojureFunction(
							"reify",
							"java.util.function.Function",
							ClojureHelper.applyClojureFunction(
									"apply",
									ClojureHelper.clojureVectorHelper(
											asFunctionClojureDef_this,
											asFunctionClojureDef_arg
											),
									ClojureHelper.applyClojureFunction(
											asFunctionClojureDef_f,
											asFunctionClojureDef_arg))));
	
	private static final String getCostFunctionDef_fun = "_fun";
	public static final String getCostFunctionDef = 
			ClojureHelper.clojureDefnHelper(ClojureCoreSymbols.getCostFunction,
					Arrays.asList(getCostFunctionDef_fun),
					ClojureHelper.applyClojureFunction(
							ClojureCoreSymbols.costFunctionKey,
							ClojureHelper.applyClojureFunction(
									"meta",
									getCostFunctionDef_fun)));

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
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.asFunctionClojure));
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.getCostFunction));
	
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
		sb.append("\n");
		sb.append(asFunctionClojureDef);
		sb.append("\n");
		sb.append(getCostFunctionDef);
	
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
