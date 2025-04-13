package velka.clojure;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

import velka.core.expression.Tuple;
import velka.core.literal.LitComposite;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.typeSystem.VelkaAbstraction;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.ClojureHelper.ProxyImpl;
import velka.util.Pair;

/**
 * This class is generating velka.runtime.core namespace file velka/clojure/core.clj
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
	
	private static final String getTypeClojure_expr = "_expr", declared = "_declared";
	/**
	 * Definition for get-type clojure symbol
	 */
	public static String getTypeClojureDef = 
			ClojureHelper.clojureDefnHelper(ClojureCoreSymbols.getTypeClojureSymbol, List.of(getTypeClojure_expr), 
					ClojureHelper.applyClojureFunction(".getType", ClojureCoreSymbols.typeSystem_full, getTypeClojure_expr));
					
	
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
	
	private final static String me = "_me", ts = "_type-system", from = "_from", to = "_to", o = "_o", exp = "_exp", impl = "_impl", arg = "_arg", toNormal = "_toNormal", env = "_env";
	
	/** Definition for convert-tuple clojure function */
	public static String convertTupleClojureDef = ClojureHelper.clojureDefnHelper(
			ClojureCoreSymbols.convertTupleClojureSymbol,
			Arrays.asList(to, exp),
			ClojureHelper.addTypeMetaInfo_str(
					ClojureHelper.applyClojureFunction(
							"vec",
							ClojureHelper.applyClojureFunction(
									"map",
									ClojureCoreSymbols.convertClojureSymbol,
									to,
									exp)),
					to));
	
	private static String lunify = "_lunify";
	private static String runify = "_runify";
	private static String from_ltype = "_from_ltype";
	private static String from_rtype = "_from_rtype";
	private static String to_ltype = "_to_ltype";
	private static String to_rtype = "_to_rtype";
	private static final String type = "_type";
	private static final String args = "_args";
	
	/** Definition for convert-fn clojure function */
	public static String convertFnClojureDef = ClojureHelper.clojureDefnHelper(ClojureCoreSymbols.convertFnClojureSymbol, 
			Arrays.asList(from, to, exp), 
			ClojureHelper.letHelper(
					impl,
					Pair.of(from_ltype, ClojureHelper.applyClojureFunction(".ltype", from)),
					Pair.of(from_rtype, ClojureHelper.applyClojureFunction(".rtype", from)),
					Pair.of(to_ltype, ClojureHelper.applyClojureFunction(".ltype", to)),
					Pair.of(to_rtype, ClojureHelper.applyClojureFunction(".rtype", to)),
					Pair.of(lunify, 
							ClojureHelper.applyClojureFunction(".isPresent",
									ClojureHelper.applyClojureFunction("velka.types.Type/unifyRepresentation", 
											from_ltype,
											to_ltype))),
					Pair.of(runify, 
							ClojureHelper.applyClojureFunction(".isPresent",
									ClojureHelper.applyClojureFunction("velka.types.Type/unifyRepresentation", 
											from_rtype,
											to_rtype))),
					Pair.of(impl, 
							ClojureHelper.condHelper(
									Pair.of(ClojureHelper.applyClojureFunction("and", lunify, runify), exp),
									Pair.of(lunify, 
											ClojureHelper.letHelper(
												ClojureHelper.reify(
														velka.types.typeSystem.VelkaAbstraction.class,
														Pair.of("apply", Pair.of(List.of(me, args), ClojureHelper.applyClojureFunction(".convert",
																ClojureCoreSymbols.typeSystem, from_rtype, to_rtype, 
																ClojureHelper.applyClojureFunction(".apply", exp, args), "nil"))),
														Pair.of("getType", Pair.of(List.of(me), type))),
												Pair.of(type, ClojureHelper.constructJavaClass(TypeArrow.class, from_ltype, to_rtype)))),
									Pair.of(runify,
											ClojureHelper.letHelper(
													ClojureHelper.reify(
															velka.types.typeSystem.VelkaAbstraction.class,
															Pair.of("apply", Pair.of(List.of(me, args), 
																	ClojureHelper.applyClojureFunction(".apply", exp, 
																			ClojureHelper.applyClojureFunction(".convert", ClojureCoreSymbols.typeSystem, 
																					to_ltype, from_ltype, args, "nil")))),
															Pair.of("getType", Pair.of(List.of(me), type))),
													Pair.of(type, ClojureHelper.constructJavaClass(TypeArrow.class, to_ltype, from_rtype)))),
									Pair.of(":else",
											ClojureHelper.reify(
													velka.types.typeSystem.VelkaAbstraction.class,
													Pair.of("apply", Pair.of(List.of(me, args), ClojureHelper.applyClojureFunction(".convert",
															ClojureCoreSymbols.typeSystem, from_rtype, to_rtype, 
															ClojureHelper.applyClojureFunction(".apply", exp,
																	ClojureHelper.applyClojureFunction(".convert", ClojureCoreSymbols.typeSystem,
																			to_ltype, from_ltype, args, "nil")), "nil"))),
													Pair.of("getType", Pair.of(List.of(me), to))))))));
	
	/** Definition of clojure type system */
	public static String typeSystem = ClojureHelper.dynamicDef(
			ClojureCoreSymbols.typeSystem_full, 
			ClojureHelper.proxy(velka.types.typeSystem.TypeSystem.class,
				List.of(ClojureHelper.reify(velka.types.typeSystem.IConversionEngine.class, 
							Pair.of("convertTuple", Pair.of(List.of(me, ts, from, to, o, env), 
									ClojureHelper.applyClojureFunction(ClojureCoreSymbols.convertTupleClojureSymbol_full, to, o))),
							Pair.of("convertFunction", Pair.of(List.of(me, ts, from, to, o, env), 
									ClojureHelper.applyClojureFunction(ClojureCoreSymbols.convertFnClojureSymbol_full, from, to, o))),
							Pair.of("instantiateCollection", Pair.of(List.of(me, o), ClojureHelper.tupleHelper(o))))),
				ProxyImpl.of(velka.types.typeSystem.TypeSystem.class,
						"getType",
						List.of(arg),
						ClojureHelper.letHelper(
								ClojureHelper.clojureIfHelper(ClojureHelper.applyClojureFunction("some?", declared),
										declared,
										ClojureHelper.condHelper(
												Pair.of(ClojureHelper.applyClojureFunction("nil?", arg), TypeTuple.EMPTY_TUPLE.clojureTypeRepresentation()),
												Pair.of(ClojureHelper.applyClojureFunction("seq?", arg), TypeAtom.TypeListNative.clojureTypeRepresentation()),
												Pair.of(ClojureHelper.applyClojureFunction("instance?", "java.util.Collection", arg), TypeAtom.TypeListNative.clojureTypeRepresentation()),
												Pair.of(":else", ClojureHelper.errorHelper(ClojureHelper.stringHelper("Unrecognized type in Clojure."))))),
								Pair.of(declared, ClojureHelper.applyClojureFunction(":lang-type", 
														ClojureHelper.applyClojureFunction("meta", arg))),
								Pair.of(declared, ClojureHelper.clojureIfHelper(ClojureHelper.applyClojureFunction("nil?", declared), 
													ClojureHelper.applyClojureFunction("velka.types.TypeAtom/javaClassToType", 
																ClojureHelper.applyClojureFunction(".getClass", arg)),
													declared)),
								Pair.of(declared, ClojureHelper.clojureIfHelper(ClojureHelper.isInstanceOfClass(arg, VelkaAbstraction.class),
																	ClojureHelper.applyClojureFunction(".getType", arg),
																	declared))),
						Object.class)));
			
	/**
	 * Definition for convert clojure function
	 */
	public static String convertClojureDef = 
			ClojureHelper.clojureDefnHelper(
					ClojureCoreSymbols.convertClojureSymbol,
					List.of(to, exp),
					ClojureHelper.applyClojureFunction(".convert", 
							ClojureCoreSymbols.typeSystem_full, 
							ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, exp),
							to,
							exp, 
							"nil"));
	
	//(defn can-convert-atom ([_from _to]
	//					 	  (or (= _from _to)
	//							  (instance? _to velka.types.TypeVariable)
	//							  (and (= (.type _from) (.type _to))
	//								   (or (= (.representation _from) velka.types.TypeRepresentation/WILDCARD)
	//									   (= (.representation _to) velka.types.TypeRepresentation/WILDCARD)
	//							  		   (contains? ClojureCoreSymbols.atomicConversionMapClojureSymbol [_from _to]))))))
	
	public static final String canConvertDef = 
			ClojureHelper.clojureDefnHelper(ClojureCoreSymbols.canConvert, List.of(from, to),
					ClojureHelper.applyClojureFunction(".canConvert", 
							ClojureCoreSymbols.typeSystem_full, from, to));
	
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
	private static final String x = "_x";
	public static final String conversionCostDef = 
			ClojureHelper.clojureDefnHelper(ClojureCoreSymbols.conversionCost, List.of(from, to, exp), 
					ClojureHelper.applyClojureFunction(".conversionCost",
							ClojureCoreSymbols.typeSystem_full,
							from, to, exp, "nil"));	
	
	private static final String icost = "_icost", ccost = "_ccost";
	//(defn impl-cost ([_impl _args]
	//				   (let [_icost (eapply (getcost _impl) _args)
	//						 _ccost (conversion-cost (get-type _args) (.ltype (get-type _impl)) _args)]
	//					(+ _icost _ccost))))
	
	public static final String implementationCost = 
			ClojureHelper.clojureDefnHelper(ClojureCoreSymbols.implementationCost, List.of(impl, args), 
					ClojureHelper.letHelper(ClojureHelper.clojureIfHelper(ClojureHelper.applyClojureFunction("nil?", ccost), 
							"nil",
							ClojureHelper.applyClojureFunction(".aggregate", ClojureHelper.applyClojureFunction("velka.util.RankAggregation/instance"), icost, ccost)), 
							Pair.of(icost, ClojureHelper.applyVelkaFunction_argsTuple(ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getCostFunction_full, impl), args)),
							Pair.of(ccost, ClojureHelper.applyClojureFunction(ClojureCoreSymbols.conversionCost_full, 
									ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, args),
									ClojureHelper.applyClojureFunction(".ltype", ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, impl)),
									args))));
	
	private static final String efun = "_efun", p1 = "_p1", p2 = "_p2";
	public static final String selectImplementation =
			ClojureHelper.clojureDefnHelper(ClojureCoreSymbols.selectImplementationClojureSymbol, List.of(efun, args), 
					ClojureHelper.applyClojureFunction("first", 
							ClojureHelper.applyClojureFunction("reduce", 
									ClojureHelper.fnHelper(List.of(p1, p2), ClojureHelper.clojureIfHelper(
											ClojureHelper.applyClojureFunction("or", ClojureHelper.applyClojureFunction(">", 
														ClojureHelper.applyClojureFunction("second", p2),
														ClojureHelper.applyClojureFunction("second", p1)),
													ClojureHelper.applyClojureFunction("nil?", ClojureHelper.applyClojureFunction("first", p1))), 
											p2, p1)),
									ClojureHelper.clojureVectorHelper("nil", "0"),
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
													new Pair<String, String>(ClojureHelper.applyClojureFunction("nil?", langPstrClojureDef_expr), "[]"),
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
	
	public static final String cmdArgs = ClojureHelper.def(ClojureCoreSymbols.CONSOLE_ARGS_SYMBOL, 
			ClojureHelper.clojureIfHelper(ClojureHelper.applyClojureFunction("some?", ClojureHelper.COMMAND_LINE_ARGS), 
					ClojureHelper.applyClojureFunction("vec", ClojureHelper.tupleHelper_str("*command-line-args*")),
					Tuple.EMPTY_TUPLE_CLOJURE));

	/**
	 * Generates clojure code for definitions of velka.runtime.core namespace
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
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.typeSystem));
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.convertTupleClojureSymbol));
		sb.append(ClojureHelper.makeDeclaration(ClojureCoreSymbols.convertFnClojureSymbol));
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
		//sb.append(tuple2velkaListDef);
		//sb.append("\n");
		sb.append(getTypeClojureDef);
		sb.append("\n");
		sb.append(listNativeToTuple);
		sb.append("\n");
		sb.append(convertTupleClojureDef);
		sb.append("\n");
		sb.append(convertFnClojureDef);
		sb.append("\n");
		sb.append(typeSystem);
		sb.append("\n");
		sb.append(convertClojureDef);
		sb.append("\n");
		sb.append(eapply);
		sb.append("\n");
		sb.append(canDeconstructAsDef);
		sb.append("\n");
		sb.append(conversionCostDef);
		sb.append("\n");
		sb.append(implementationCost);
		sb.append("\n");
		sb.append(selectImplementation);
		sb.append("\n");
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
	 * Relative path to velka.runtime.core file
	 */
	public static final Path VELKA_CLOJURE_CORE_PATH = Paths.get("velka", "runtime");
	
	/**
	 * Name of the velka.runtime.core file
	 */
	public static final Path VELKA_CLOJURE_CORE_NAME = Paths.get("core.clj");

	/**
	 * Relative path to file
	 */
	public static final Path RELATIVE_PATH = VELKA_CLOJURE_CORE_PATH.resolve(VELKA_CLOJURE_CORE_NAME);
	
}
