package velka.clojure;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.TreeMap;
import java.util.function.BiFunction;

import velka.core.application.CanDeconstructAs;
import velka.core.langbase.ConversionOperators;
import velka.core.literal.LitBoolean;
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
					ClojureHelper.condHelper(
							Pair.of(ClojureHelper.applyClojureFunction("integer?", getTypeClojure_expr), TypeAtom.TypeIntNative.clojureTypeRepresentation()),
							Pair.of(ClojureHelper.applyClojureFunction("boolean?", getTypeClojure_expr), TypeAtom.TypeBoolNative.clojureTypeRepresentation()),
							Pair.of(ClojureHelper.applyClojureFunction("double?", getTypeClojure_expr), TypeAtom.TypeDoubleNative.clojureTypeRepresentation()),
							Pair.of(ClojureHelper.applyClojureFunction("string?", getTypeClojure_expr), TypeAtom.TypeStringNative.clojureTypeRepresentation()),
							Pair.of(ClojureHelper.applyClojureFunction("instance?", java.util.ArrayList.class.getName(), getTypeClojure_expr), TypeAtom.TypeListJavaArray.clojureTypeRepresentation()),
							Pair.of(ClojureHelper.applyClojureFunction("instance?", java.util.LinkedList.class.getName(), getTypeClojure_expr), TypeAtom.TypeListJavaLinked.clojureTypeRepresentation()),
							Pair.of(ClojureHelper.applyClojureFunction("instance?", java.util.BitSet.class.getName(), getTypeClojure_expr), TypeAtom.TypeSetBitSet.clojureTypeRepresentation()),
							Pair.of(ClojureHelper.applyClojureFunction("instance?", java.util.TreeMap.class.getName(), getTypeClojure_expr), velka.types.TypeAtom.TypeMapTree.clojureTypeRepresentation()),
							Pair.of(ClojureHelper.applyClojureFunction("instance?", java.util.Scanner.class.getName(), getTypeClojure_expr), velka.types.TypeAtom.TypeScannerNative.clojureTypeRepresentation()),
							Pair.of(ClojureHelper.applyClojureFunction("instance?", "clojure.lang.LazySeq", getTypeClojure_expr), TypeAtom.TypeListNative.clojureTypeRepresentation()),
							Pair.of(ClojureHelper.applyClojureFunction("instance?", java.util.ListIterator.class.getName(), getTypeClojure_expr), TypeAtom.TypeListIterator.clojureTypeRepresentation()),
							Pair.of(":else", ClojureHelper.applyClojureFunction(":lang-type", 
									ClojureHelper.applyClojureFunction("meta", getTypeClojure_expr)))));
	
	private static final String ListNativeToTuple_list = "_list";
	/**
	 * Definition of list-native-to-tuple function
	 */
	public static String listNativeToTuple =	
			ClojureHelper
					.clojureDefnHelper(ClojureCoreSymbols.listNativeToTuple, Arrays.asList(ListNativeToTuple_list),
							ClojureHelper.tupleHelper_str(ListNativeToTuple_list));
	
	
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
	
	//(defn can-convert-atom ([_from _to]
	//					 	  (or (= _from _to)
	//							  (instance? _to velka.types.TypeVariable)
	//							  (and (= (.type _from) (.type _to))
	//								   (or (= (.representation _from) velka.types.TypeRepresentation/WILDCARD)
	//									   (= (.representation _to) velka.types.TypeRepresentation/WILDCARD)
	//							  		   (contains? ClojureCoreSymbols.atomicConversionMapClojureSymbol [_from _to]))))))
	
	private final static String from = "_from";
	private final static String to = "_to";
	public static final String canConvertAtomDef = 
			ClojureHelper.clojureDefnHelper(ClojureCoreSymbols.canConvert, List.of(from, to),
					ClojureHelper.applyClojureFunction("or", 
							ClojureHelper.applyClojureFunction("=", from, to),
							ClojureHelper.applyClojureFunction("instance?", "velka.types.TypeVariable", to),
							ClojureHelper.applyClojureFunction("and", 
									ClojureHelper.applyClojureFunction("=", 
											ClojureHelper.applyClojureFunction(".name", from), 
											ClojureHelper.applyClojureFunction(".name", to)),
									ClojureHelper.applyClojureFunction("or", 
											ClojureHelper.applyClojureFunction("=", 
													ClojureHelper.applyClojureFunction(".representation", from),
													"velka.types.TypeRepresentation/WILDCARD"),
											ClojureHelper.applyClojureFunction("=", 
													ClojureHelper.applyClojureFunction(".representation", to),
													"velka.types.TypeRepresentation/WILDCARD"),
											ClojureHelper.applyClojureFunction("contains?", 
													ClojureCoreSymbols.atomicConversionMapClojureSymbol_full,
													ClojureHelper.clojureVectorHelper(from, to))))));
	
	//(defn conv-cost-fn ([_from _to]
	//					  (TypeAtom.conversionCostCljMetaName (meta (get ClojureCoreSymbols.atomicConversionMapClojureSymbol_full [_from _to])))))
	
	public static final String conversionCostFn =
			ClojureHelper.clojureDefnHelper(ClojureCoreSymbols.conversionCostFn, List.of(from, to), 
					ClojureHelper.applyClojureFunction(TypeAtom.conversionCostCljMetaName, 
							ClojureHelper.applyClojureFunction("meta", 
									ClojureHelper.applyClojureFunction("get",
											ClojureCoreSymbols.atomicConversionMapClojureSymbol_full,
											ClojureHelper.clojureVectorHelper(from, to)))));
	
	
	//(defn conv-cost ([_from _to _exp]
	//				   (cond (= _from _to) 0
	//						 (not (can-convert _from _to)) nil
	//						 (or (instance? velka.types.TypeVariable _to)
	//							 (instance? velka.types.RepresentationOr _from)
	//							 (instance? velka.types.RepresentationOr _to)) 0
	//						 (instance? _from velka.types.TypeAtom) (eapply (TypeAtom.conversionCostCljMetaName (get ClojureCoreSymbols.atomicConversionMapClojureSymbol_full [_from _to])) [_exp])
	//						 (instance? _from velka.types.TypeTuple) (reduce (fn [x y] (if (nil? y) nil (+ x y))) 0 (map conv-cost _from _to _exp))
	//						 (instance? _from velka.types.TypeArrow) 1
	//						 :else (throw "cannot compute conversion cost"))))
	
	private static final String exp = "_exp";
	private static final String x = "_x";
	private static final String y = "_y";
	public static final String conversionCostDef =
			ClojureHelper.clojureDefnHelper(ClojureCoreSymbols.conversionCost, List.of(from, to, exp), 
					ClojureHelper.condHelper(Pair.of(ClojureHelper.applyClojureFunction("=", from, to), "0"),
											 Pair.of(ClojureHelper.applyClojureFunction("not", 
													 ClojureHelper.applyClojureFunction("velka.types.Type/canConvert", 
															 from, 
															 to,
															 ClojureHelper.applyClojureFunction("reify", 
																	 BiFunction.class.getName(), 
																	 ClojureHelper.applyClojureFunction("apply", 
																			 ClojureHelper.clojureVectorHelper("this", x, y), 
																			 ClojureHelper.applyClojureFunction(ClojureCoreSymbols.canConvert_full, x, y))))), "nil"),
											 Pair.of(ClojureHelper.applyClojureFunction("or", 
													 ClojureHelper.isInstanceOfClass(to, TypeVariable.class), 
													 ClojureHelper.isInstanceOfClass(from, RepresentationOr.class),
													 ClojureHelper.isInstanceOfClass(to, RepresentationOr.class)), "0"),
											 Pair.of(ClojureHelper.isInstanceOfClass(from, TypeAtom.class), ClojureHelper.applyVelkaFunction(ClojureHelper.applyClojureFunction(ClojureCoreSymbols.conversionCostFn_full, 
													 from, to),
													 exp)),
											 Pair.of(ClojureHelper.isInstanceOfClass(from, TypeTuple.class), ClojureHelper.applyClojureFunction("reduce", 
													 ClojureHelper.fnHelper(List.of(x, y), ClojureHelper.clojureIfHelper(ClojureHelper.applyClojureFunction("nil?", y), "nil", ClojureHelper.applyClojureFunction("+", x, y))),
													 "0",
													 ClojureHelper.applyClojureFunction("map", ClojureCoreSymbols.conversionCost_full, from, to, exp))),
											 Pair.of(ClojureHelper.isInstanceOfClass(from, TypeArrow.class), "1"),
											 Pair.of(":else", ClojureHelper.errorHelper(ClojureHelper.applyClojureFunction("str", ClojureHelper.stringHelper("cannot compute conversion cost"), from, to, exp)))));
	
	
	private static final String impl = "_impl", args = "_args", icost = "_icost", ccost = "_ccost";
	//(defn impl-cost ([_impl _args]
	//				   (let [_icost (eapply (getcost _impl) _args)
	//						 _ccost (conversion-cost (get-type _args) (.ltype (get-type _impl)) _args)]
	//					(+ _icost _ccost))))
	
	public static final String implementationCost = 
			ClojureHelper.clojureDefnHelper(ClojureCoreSymbols.implementationCost, List.of(impl, args), 
					ClojureHelper.letHelper(ClojureHelper.clojureIfHelper(ClojureHelper.applyClojureFunction("nil?", ccost), 
							"nil",
							ClojureHelper.applyClojureFunction("+", icost, ccost)), 
							Pair.of(icost, ClojureHelper.applyVelkaFunction_argsTuple(ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getCostFunction_full, impl), args)),
							Pair.of(ccost, ClojureHelper.applyClojureFunction(ClojureCoreSymbols.conversionCost_full, 
									ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, args),
									ClojureHelper.applyClojureFunction(".ltype", ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, impl)),
									args))));
	
	//(defn select-impl ([_efun _args]
	//					(first (reduce 
	//						(fn [p1 p2] (if (or (< (second p2) (second p1)) (nil? (first p1))) p2 p1))
	//						[nil java.lang.Long/MAX_VALUE] 
	//						(filter (fn [_x] (not (nil? (second _x)))) (map (fn [_impl] [_impl (implementation-cost _impl _args)]) _efun)))))))
	
	private static final String efun = "_efun", p1 = "_p1", p2 = "_p2";
	public static final String selectImplementation =
			ClojureHelper.clojureDefnHelper(ClojureCoreSymbols.selectImplementationClojureSymbol, List.of(efun, args), 
					ClojureHelper.applyClojureFunction("first", 
							ClojureHelper.applyClojureFunction("reduce", 
									ClojureHelper.fnHelper(List.of(p1, p2), ClojureHelper.clojureIfHelper(
											ClojureHelper.applyClojureFunction("or", ClojureHelper.applyClojureFunction("<", 
														ClojureHelper.applyClojureFunction("second", p2),
														ClojureHelper.applyClojureFunction("second", p1)),
													ClojureHelper.applyClojureFunction("nil?", ClojureHelper.applyClojureFunction("first", p1))), 
											p2, p1)),
									ClojureHelper.clojureVectorHelper("nil", "java.lang.Long/MAX_VALUE"),
									ClojureHelper.applyClojureFunction("filter", 
											ClojureHelper.fnHelper(List.of(x), ClojureHelper.applyClojureFunction("not", ClojureHelper.applyClojureFunction("nil?", ClojureHelper.applyClojureFunction("second", x)))),
											ClojureHelper.applyClojureFunction("map", 
													ClojureHelper.fnHelper(List.of(impl), 
															ClojureHelper.clojureVectorHelper(impl, ClojureHelper.applyClojureFunction(ClojureCoreSymbols.implementationCost_full, impl, args))),
													efun)))));
	
	
	//(defn eapply ([_abstr _args]
	//				(let [_i (cond (fn? _abstr) _abstr 
	//							   (set? _abstr) (select-implementation _abstr _args))
	//					  _cargs (convert (.ltype (get-type _i)) _args)
	//					 ]
	//					(apply _i _cargs))))
	
	private static final String abstr = "_abstr", cargs = "_cargs";
	public static final String eapply = 
			ClojureHelper.clojureDefnHelper(ClojureCoreSymbols.eapplyClojureSymbol, List.of(abstr, args), 
					ClojureHelper.letHelper(ClojureHelper.applyClojureFunction("apply", impl, cargs), 
							Pair.of(impl, ClojureHelper.condHelper(
									Pair.of(ClojureHelper.applyClojureFunction("fn?", abstr), abstr),
									Pair.of(ClojureHelper.applyClojureFunction("set?", abstr), ClojureHelper.applyClojureFunction(ClojureCoreSymbols.selectImplementationClojureSymbol_full, abstr, args)))),
							Pair.of(cargs, ClojureHelper.applyClojureFunction(ClojureCoreSymbols.convertClojureSymbol_full, 
									ClojureHelper.applyClojureFunction(".ltype", 
											ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, impl)),
									args))));
	
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
																	ClojureHelper.applyClojureFunction("pr-str", langPstrClojureDef_expr), 
																	langPstrClojureDef_expr)),
													new Pair<String, String>(
															ClojureHelper.applyClojureFunction("=",
																	langPstrClojureDef_type,
																	"velka.types.TypeAtom/TypeSetBitSet"),
															ClojureHelper.applyClojureFunction(".toString",
																	langPstrClojureDef_expr)),
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
																	langPstrClojureDef_expr,
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
	
	private static final String arg = "_arg";
	private static final String type = "_type";
	public static final String canDeconstructAsDef =
			ClojureHelper.clojureDefnHelper(
					ClojureCoreSymbols.canDeconstructAs, 
					List.of(arg, type), 
					ClojureHelper.clojureIfHelper(
							ClojureHelper.applyClojureFunction("vector?", arg),
							ClojureHelper.applyClojureFunction(".isPresent", 
									ClojureHelper.applyClojureFunction("velka.types.Type/unifyRepresentation", 
											type,
											ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full,
													ClojureHelper.applyClojureFunction("first", arg)))),
							"false"));

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
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.canDeconstructAs));
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.canConvert));
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.conversionCostFn));
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.conversionCost));
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.implementationCost));
	
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
		sb.append(eapply);
		sb.append("\n");
		sb.append(canDeconstructAsDef);
		sb.append("\n");
		sb.append(canConvertAtomDef);
		sb.append("\n");
		sb.append(conversionCostFn);
		sb.append("\n");
		sb.append(conversionCostDef);
		sb.append("\n");
		sb.append(implementationCost);
		sb.append("\n");
		sb.append(selectImplementation);
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
