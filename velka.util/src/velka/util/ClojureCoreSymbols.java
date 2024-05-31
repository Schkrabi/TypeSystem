package velka.util;


public class ClojureCoreSymbols {

	/**
	 * Namespace for velka.clojure.core
	 */
	public static String NAMESPACE = "velka.clojure.core";
	
	/**
	 * Symbol for list-native-to-tuple
	 */
	public static final String listNativeToTuple = "list-native-to-tuple";
	/**
	 * Fully qualified symbol for list-native-to-tuple
	 */
	public static final String listNativeToTuple_full = ClojureHelper.fullyQualifySymbol(ClojureCoreSymbols.NAMESPACE, listNativeToTuple);
	/**
	 * Symbol for type-2-type-symbol function
	 */
	public static String type2typeSymbolSymbol = "type-2-typesymbol";
	/**
	 * Fully qualified type-2-type-symbol function symbol
	 */
	public static String type2typeSymbolSymbol_full = ClojureHelper.fullyQualifySymbol(ClojureCoreSymbols.NAMESPACE, type2typeSymbolSymbol);
	/**
	 * Symbol for get-type clojure symbol
	 */
	public static String getTypeClojureSymbol = "get-type";
	/**
	 * Fully qualified get-type clojure symbol
	 */
	public static String getTypeClojureSymbol_full = ClojureHelper.fullyQualifySymbol(ClojureCoreSymbols.NAMESPACE, getTypeClojureSymbol);
	/**
	 * Symbol for tuple-2-velka-list function
	 */
	public static String tuple2velkaListSymbol = "tuple-2-list";
	/**
	 * Fully qualified tuple-2-velka-list symbol
	 */
	public static String tuple2velkaListSymbol_full = ClojureHelper.fullyQualifySymbol(ClojureCoreSymbols.NAMESPACE, tuple2velkaListSymbol);
	/**
	 * Symbol for map of atomic type conversion
	 */
	public static String atomicConversionMapClojureSymbol = "*ATOMIC-CONVERSION-MAP*";
	/**
	 * Fully qualified atomic-conversion-map symbol
	 */
	public static String atomicConversionMapClojureSymbol_full = ClojureHelper.fullyQualifySymbol(ClojureCoreSymbols.NAMESPACE, atomicConversionMapClojureSymbol);
	/**
	 * Symbol for convert-type-atom clojure function
	 */
	public static String convertAtomClojureSymbol = "convert-type-atom";
	/**
	 * Fully qualified convert-type-atom clojure symbol
	 */
	public static String convertAtomClojureSymbol_full = ClojureHelper.fullyQualifySymbol(ClojureCoreSymbols.NAMESPACE, convertAtomClojureSymbol);
	/**
	 * Symbol for convert-tuple clojure symbol
	 */
	public static String convertTupleClojureSymbol = "convert-tuple";
	/**
	 * Fully qualified convert-tuple clojure symbol
	 */
	public static String convertTupleClojureSymbol_full = ClojureHelper.fullyQualifySymbol(ClojureCoreSymbols.NAMESPACE, convertTupleClojureSymbol);
	/**
	 * Symbol for convert-fn clojure function
	 */
	public static String convertFnClojureSymbol = "convert-fn";
	/**
	 * Fully qualified convert-fn symbol
	 */
	public static String convertFnClojureSymbol_full = ClojureHelper.fullyQualifySymbol(ClojureCoreSymbols.NAMESPACE, convertFnClojureSymbol);
	/**
	 * Symbol for convert-set clojure function
	 */
	public static String convertRepOrClojureSymbol = "convert-set";
	/**
	 * Fully qualified symbol convert-set
	 */
	public static String convertRepOrClojureSymbol_full = ClojureHelper.fullyQualifySymbol(ClojureCoreSymbols.NAMESPACE, convertRepOrClojureSymbol);
	/**
	 * Symbol for convert-to-set clojure function
	 */
	public static String convertToRepOrClojureSymbol = "convert-to-set";
	/**
	 * Fully qualified symbol convert-to-set
	 */
	public static String convertToRepOrClojureSymbol_full = ClojureHelper.fullyQualifySymbol(ClojureCoreSymbols.NAMESPACE, convertToRepOrClojureSymbol);
	/**
	 * Symbol for convert-from-set clojure function
	 */
	static String convertFromRepOrClojureSymbol = "convert-from-set";
	/**
	 * Fully qualified symbol convert-from-set
	 */
	public static String convertFromRepOrClojureSymbol_full = ClojureHelper.fullyQualifySymbol(ClojureCoreSymbols.NAMESPACE, convertFromRepOrClojureSymbol);
	/**
	 * Symbol for convert clojure function
	 */
	public static String convertClojureSymbol = "convert";
	/**
	 * Fully qualified symbol convert
	 */
	public static String convertClojureSymbol_full = ClojureHelper.fullyQualifySymbol(ClojureCoreSymbols.NAMESPACE, convertClojureSymbol);
	/**
	 * Symbol for eapply function
	 */
	public static String eapplyClojureSymbol = "eapply";
	/**
	 * Fully qualified eapply symbol
	 */
	public static String eapplyClojureSymbol_full = ClojureHelper.fullyQualifySymbol(ClojureCoreSymbols.NAMESPACE, eapplyClojureSymbol);
	/**
	 * Symbol for select-implementation-clojure function
	 */
	public static String selectImplementationClojureSymbol = "select-implementation";
	/**
	 * Fully qualify select-implementation-clojure symbol
	 */
	public static String selectImplementationClojureSymbol_full = ClojureHelper.fullyQualifySymbol(ClojureCoreSymbols.NAMESPACE, selectImplementationClojureSymbol);
	/**
	 * Symbol for lang-pstr clojure function
	 */
	public static final String langPstrClojure = "lang-pstr";
	/**
	 * Fully qualified symbol form lang-pstr clojure function
	 */
	public static final String langPstrClojure_full = ClojureHelper.fullyQualifySymbol(ClojureCoreSymbols.NAMESPACE, langPstrClojure);
	
	/**
	 * Symbol for as-function clojure function
	 */
	public static final String asFunctionClojure = "as-function";	
	/**
	 * Fully qualified symbol for as-function clojure function 
	 */
	public static final String asFunctionClojure_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, asFunctionClojure);

	public static final String costFunctionKey = ":cost-fn";
	
	/**
	 * Symbol for get-cost-function clojure function
	 */
	public static final String getCostFunction = "get-cost-function";
	/**
	 * Fully qualified symbol for get-cost-function clojure function
	 */
	public static final String getCostFunction_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, getCostFunction);
	
	public static final String canDeconstructAs = "can-deconstruct-as";
	public static final String canDeconstructAs_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, canDeconstructAs);
	
	public static final String canConvert = "can-convert";
	public static final String canConvert_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, canConvert);
	
	public static final String conversionCostFn = "conversion-cost-fn";
	public static final String conversionCostFn_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, conversionCostFn);
	
	public static final String conversionCost = "conversion-cost";
	public static final String conversionCost_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, conversionCost);
	
	public static final String implementationCost = "implementation-cost";
	public static final String implementationCost_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, implementationCost);
}
