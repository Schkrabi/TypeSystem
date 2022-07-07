package velka.util;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Class containing static utilities used to help with generating clojure code
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class ClojureHelper {

	/**
	 * Adds type meta information to given clojure code piece
	 * 
	 * @param cljCode code to put the meta information to
	 * @param type    type
	 * @return clojure code with meta information
	 */
	public static String addTypeMetaInfo_str(String cljCode, String typeInfo) {
		String code = addMetadata(cljCode, new Pair<String, String>(":lang-type", typeInfo));
		
		return code;
	}

	/**
	 * Creates fully qualified velka.clojure.core symbol from symbol
	 * 
	 * @param symbol non-qualified symbol
	 * @return fully qualified symbol
	 */
	public static String fullyQualifySymbol(String namespace, String symbol) {
		return namespace + "/" + symbol;
	}

	/**
	 * Creates clojure declaration for given symbol
	 * 
	 * @param symbol declared symbol
	 * @return string with code
	 */
	public static String makeDeclaration(String symbol) {
		String code = applyClojureFunction("declare", symbol);
		return code;
	}
	
	/**
	 * Creates dynamic declaration for given symbol
	 * 
	 * @param symbol declared symbol
	 * @return string with code
	 */
	public static String makeDynamicDeclaration(String symbol) {
		return applyClojureFunction("declare", "^:dynamic", symbol);
	}
	
	/**
	 * Creates a dynamic definition for given symbol to given value
	 * @param symbol defined symbol
	 * @param value binded value
	 * @return string with Clojure code
	 */
	public static String dynamicDef(String symbol, String value) {
		return applyClojureFunction("def", "^:dynamic", symbol, value);
	}

	/**
	 * Creates clojure namespace declaration
	 * 
	 * @param namespace name of the namespace
	 * @return string with code
	 */
	public static String declareNamespace(String namespace) {
		return "(ns " + namespace + " (:gen-class))\n";
	}

	/**
	 * Requires given namespace in clojure code
	 * 
	 * @param namespace required namespace
	 * @return string with code
	 */
	public static String requireNamespace(String namespace) {
		return "(require '[" + namespace + "])\n";
	}

	public static String clojureVectorHelper(Collection<String> members) {
		StringBuilder sb = new StringBuilder("[");
		
		Iterator<String> i = members.iterator();
		while(i.hasNext()) {
			String member = i.next();
			sb.append(member);
			if(i.hasNext()) {
				sb.append(" ");
			}
		}
		
		sb.append("]");
		return sb.toString();
	}
	
	/**
	 * Creates clojure PersistentVector from arguments
	 * @param members vector members
	 * @return string with code
	 */
	public static String clojureVectorHelper(String ...members) {
		return ClojureHelper.clojureVectorHelper(Arrays.asList(members));
	}
	
	/**
	 * Creates velka tuple from arguments
	 * @param args tuple members
	 * @return string with code
	 */
	public static String tupleHelper(String ...args) {
		return tupleHelper_array(args);
	}
	
	/**
	 * Creates velka tuple from arguments
	 * @param members tuple members
	 * @return string with code
	 */
	public static String tupleHelper_array(String[] members) {
		return tupleHelper(Arrays.asList(members));
	}
	
	/**
	 * Creates velka tuple from arguments
	 * @param members tuple members
	 * @return string with code
	 */
	public static String tupleHelper(Collection<String> members) {
		return ClojureHelper.tupleHelper_str(ClojureHelper.clojureVectorHelper(members));
	}
	
	/**
	 * Creates a velka tuple from code returning clojure vector
	 * @param tupleCode code evaluated to clojure vector or sequence
	 * @return tuple code
	 */
	public static String tupleHelper_str(String tupleCode) {
		final String tuple = "_tuple";
		
		return ClojureHelper.letHelper(
				ClojureHelper.addTypeMetaInfo_str(tuple, "(velka.types.TypeTuple. (map " + ClojureCoreSymbols.getTypeClojureSymbol_full + " " + tuple + "))"), 
				new Pair<String, String>(tuple, tupleCode));
	}
	
	/**
	 * Applies velka function in clojure code
	 * @param funCode velka function code
	 * @param argsTuple tuple of applied arguments
	 * @param costFunction code of cost function
	 * @return string with code
	 */
	public static String applyVelkaFunction_argsTuple(String funCode, String argsTuple, String costFunction) {
		String code = applyClojureFunction(
				ClojureCoreSymbols.eapplyClojureSymbol_full,
				funCode,
				argsTuple,
				costFunction);
		return code;
	}
	
	/**
	 * Applies velka function in clojure code
	 * @param funCode velka function code
	 * @param argsTuple tuple of applied arguments
	 * @return string with code
	 */
	public static String applyVelkaFunction_argsTuple(String funCode, String argsTuple) {
		return applyVelkaFunction_argsTuple(funCode, argsTuple, "nil");
	}
	
	/**
	 * Applies velka function in clojure code
	 * @param funCode velka function code
	 * @param args arguments to apply with
	 * @return string with code
	 */
	public static String applyVelkaFunction(String funCode, String ...args)  {
		return applyVelkaFunction_argsTuple(funCode, ClojureHelper.tupleHelper(args));
	}
	
	/**
	 * Applies velka function in clojure code with cost function
	 * @param funCode funCode velka function code
	 * @param costFunction code of cost function
	 * @param args arguments to apply with
	 * @return string with code
	 */
	public static String applyVelkaFunction_cost(String funCode, String costFunction, String ...args) {
		return applyVelkaFunction_argsTuple(funCode, ClojureHelper.tupleHelper(args), costFunction);
	}
	
	/**
	 * Creates velka if expression from arguments in clojure code
	 * @param conditionCode condition
	 * @param trueCode true branch
	 * @param falseCode false branch
	 * @return string with code
	 */
	public static String velkaIfHelper(String conditionCode, String trueCode, String falseCode) {
		return clojureIfHelper(applyClojureFunction("first", conditionCode), trueCode, falseCode);
	}
	
	/**
	 * Creates if expression
	 * @param conditionCode
	 * @param trueCode
	 * @param falseCode
	 * @return
	 */
	public static String clojureIfHelper(String conditionCode, String trueCode, String falseCode) {
		return "(if " + conditionCode + " " + trueCode + " " + falseCode + ")";
	}
	
	/**
	 * Creates clojure fn expression from arguments
	 * @param fnspecs specification of all arguments-body pairs for fn
	 * @return string with code
	 */
	public static String fnHelper(List<Pair<List<String>, String>> fnspecs) {
		StringBuilder sb = new StringBuilder("(fn ");
		
		sb.append(NameGenerator.next());
		sb.append(" ");
		
		Iterator<Pair<List<String>, String>> i = fnspecs.iterator();
		while(i.hasNext()) {
			Pair<List<String>, String> p = i.next();
			List<String> args = p.first;
			String body = p.second;
			
			sb.append("(");
			sb.append(ClojureHelper.clojureVectorHelper(args));
			sb.append(" ");
			sb.append(body);
			sb.append(")");
			if(i.hasNext()) {
				sb.append(" ");
			}
		}
		
		sb.append(")");
		return sb.toString();
	}
	
	/**
	 * Creates clojure fn expression from arguments
	 * @param fnspecs specification of all arguments-body pairs for fn
	 * @return string with code
	 */
	@SafeVarargs
	public static String fnHelper(Pair<List<String>, String> ...fnspecs) {
		return ClojureHelper.fnHelper(Arrays.asList(fnspecs));
	}
	
	/**
	 * Creates clojure fn expression from arguments
	 * @param args arguments of the fn
	 * @param body body of the fn expression
	 * @return string with code
	 */
	public static String fnHelper(List<String> args, String body) {
		return ClojureHelper.fnHelper(new Pair<List<String>, String>(args, body));
	}

	/**
	 * Creates code for composite literal (LitComposite) in clojure. Allows type to be arbitrary code.
	 * @param type code returning type of the literal
	 * @param value value of the composite literal
	 * @return string with code
	 */
	public static String litCompositeHelper_str(String type, String value) {
		return addTypeMetaInfo_str("[" + value + "]", type);
	}
	
	/**
	 * Creates throw statement in clojure
	 * @param code code returning object to be thrown, probably string
	 * @return string with code
	 */
	public static String errorHelper(String code) {
		return "(throw (Throwable. " + code + "))";
	}
	
	/**
	 * Creates code for string in clojure
	 * @param str string 
	 * @return string with code
	 */
	public static String stringHelper(String str) {
		return "\"" + str + "\"";
	}
	
	/**
	 * Triplet for letfnHelper method
	 * @author Mgr. Radomir Skrabal
	 *
	 */
	public static class LetfnTriplet {
		/**
		 * Name of the function
		 */
		public final String name;
		/**
		 * arguments of the function
		 */
		public final List<String> args;
		/**
		 * body of the function
		 */
		public final String body;
		
		public LetfnTriplet(String name, List<String> args, String body) {
			this.name = name;
			this.args = args;
			this.body = body;
		}
		
		/**
		 * Creates letfn binding code
		 * @return string with code
		 */
		public String toFnspec() {
			StringBuilder sb = new StringBuilder("(");
			sb.append(this.name);
			sb.append(" ");
			
			sb.append(ClojureHelper.clojureVectorHelper(this.args));
			
			sb.append(" ");
			sb.append(this.body);
			sb.append(")");
			return sb.toString();
		}
	}
	
	/**
	 * Creates LetfnTriplet for letfnHelper method
	 * @param name name of defined function
	 * @param args arguments of defined function
	 * @param body body of defined function
	 * @return LetfnTriplet
	 */
	public static LetfnTriplet makeLetfnTriplet(String name, List<String> args, String body) {
		return new LetfnTriplet(name, args, body);
	}
	
	/**
	 * Creates clojure letfn expression
	 * @param body body of letfn
	 * @param fnspecs specifications of inner functions
	 * @see LetfnTriplet
	 * @see makeLetfnTriplet
	 * @return string with code
	 */
	public static String letfnHelper(List<LetfnTriplet> fnspecs, String body) {
		StringBuilder sb = new StringBuilder("(letfn ");
		
		sb.append(ClojureHelper
				.clojureVectorHelper(fnspecs.stream().map(x -> x.toFnspec()).collect(Collectors.toList())));
		
		sb.append(" ");
		sb.append(body);
		sb.append(")");
		return sb.toString();
	}
	
	/**
	 * Creates clojure letfn expression
	 * @param body body of letfn
	 * @param fnspecs specifications of inner functions
	 * @see LetfnTriplet
	 * @see makeLetfnTriplet
	 * @return string with code
	 */
	public static String letfnHelper(String body, LetfnTriplet ...fnspecs) {
		return letfnHelper(Arrays.asList(fnspecs), body);
	}
	
	/**
	 * Creates clojure let expression
	 * @param body body of the let expression
	 * @param defs bindings of the let, in form [variable, value]
	 * @return string with code
	 */
	public static String letHelper(String body, List<Pair<String, String>> defs) {
		StringBuilder sb = new StringBuilder("(let ");
		
		sb.append(ClojureHelper
				.clojureVectorHelper(defs.stream().map(p -> p.first + " " + p.second).collect(Collectors.toList())));
		
		sb.append(" ");
		sb.append(body);
		sb.append(")");
		return sb.toString();
	}
	
	/**
	 * Creates clojure let expression
	 * @param body body of the let expression
	 * @param defs bindings of the let, in form [variable, value]
	 * @return string with code
	 */
	@SafeVarargs
	public static String letHelper(String body, Pair<String, String> ...defs) {
		return letHelper(body, Arrays.asList(defs));
	}
	
	/**
	 * Creates a velka lambda expression in clojure from clojure fn expression
	 * @param clojureFn clojure function
	 * @return string with code
	 */
	public static String lambdaHelper(String clojureFn) {
		String impl = "_impl";
		String args = "_args";
		String cost = "_cost";
		String code = ClojureHelper.letHelper(
				ClojureHelper.fnHelper(
						new Pair<List<String>, String>(Arrays.asList(args), impl),
						new Pair<List<String>, String>(Arrays.asList(args, cost), impl)), 
				new Pair<String, String>(impl, clojureFn));
		
		return code;
	}
	
	/**
	 * Creates clojure defn expression from arguments
	 * @param name name of defined function
	 * @param args arguments of defined function
	 * @param body body of defined function
	 * @return string with code
	 */
	public static String clojureDefnHelper(String name, List<String> args, String body) {
		StringBuilder sb = new StringBuilder("(defn ");
		sb.append(name);
		sb.append(" ");
		
		sb.append(ClojureHelper.clojureVectorHelper(args));
		
		sb.append(" ");
		sb.append(body);
		sb.append(")");
		
		return sb.toString();
	}
	
	/**
	 * Creates code for clojure function application
	 * @param clojureFunction clojure function or its symbol
	 * @param args arguments
	 * @return code for application
	 */
	public static String applyClojureFunction(String clojureFunction, Collection<String> args) {
		StringBuilder sb = new StringBuilder("(");
		sb.append(clojureFunction);
		sb.append(" ");
		Iterator<String> i = args.iterator();
		while(i.hasNext()) {
			String a = i.next();
			sb.append(a);
			if(i.hasNext()) {
				sb.append(" ");
			}
		}
		sb.append(")");
		return sb.toString();
	}
	
	/**
	 * Creates code for clojure function application
	 * @param clojureFunction clojure function or its symbol
	 * @param args arguments
	 * @return code for application
	 */
	public static String applyClojureFunction(String clojureFunction, String ...args) {
		return ClojureHelper.applyClojureFunction(clojureFunction, Arrays.asList(args));
	}
	
	/**
	 * Gets inner value of literal
	 * @param literalCode
	 * @return
	 */
	public static String getLiteralInnerValue(String literalCode) {
		return ClojureHelper.applyClojureFunction("first", literalCode);
	}
	
	/**
	 * Creates clojure cond expression
	 * @param clauses pairs in from test - expression
	 * @return string with code
	 */
	public static String condHelper(Collection<Pair<String, String>> clauses) {
		StringBuilder sb = new StringBuilder("(cond ");
		
		Iterator<Pair<String, String>> i = clauses.iterator();
		while(i.hasNext()) {
			Pair<String, String> pair = i.next();
			String test = pair.first;
			String expr = pair.second;
			sb.append(test);
			sb.append(" ");
			sb.append(expr);
			if(i.hasNext()) {
				sb.append(" ");
			}
		}
		
		sb.append(")");
		return sb.toString();
	}
	
	/**
	 * Creates clojure cond expression
	 * @param clauses pairs in from test - expression
	 * @return string with code
	 */
	@SafeVarargs
	public static String condHelper(Pair<String, String> ...clauses) {
		return ClojureHelper.condHelper(Arrays.asList(clauses));
	}
	
	/**
	 * Creates code that redefines globally defined ^:dynamic variable
	 * 
	 * @param symbol redefined symbol
	 * @param value  new value
	 * @return string with code
	 */
	public static String rebindGlobalVariable(String symbol, String value) {
		return ClojureHelper.applyClojureFunction("alter-var-root", "#'" + symbol,
				ClojureHelper.applyClojureFunction("constantly", value));
	}
	
	/**
	 * Creates code representing clojure map
	 * @param bindings map bindings
	 * @return clojure code
	 */
	public static String mapHelper(Collection<Pair<String, String>> bindings) {
		StringBuilder sb = new StringBuilder();
		sb.append("{");
		
		Iterator<Pair<String, String>> i = bindings.iterator();
		while(i.hasNext()) {
			Pair<String, String> p = i.next();
			sb.append(p.first);
			sb.append(" ");
			sb.append(p.second);
			if(i.hasNext()) {
				sb.append(" ");
			}
		}
		
		sb.append("}");
		return sb.toString();
	}
	
	/**
	 * Creates code representing clojure map
	 * @param bindings map bindings
	 * @return clojure code
	 */
	@SafeVarargs
	public static String mapHelper(Pair<String, String> ...bindings) {
		return mapHelper(Arrays.asList(bindings));
	}
	
	/**
	 * Adds cost function to fn implementation
	 * @param fnCode code of implementation
	 * @param costFunctionCode code of cost function
	 * @return Clojure code
	 */
	public static String setCostFunction(String fnCode, String costFunctionCode) {
		String code = addMetadata(fnCode, new Pair<String, String>(ClojureCoreSymbols.costFunctionKey, costFunctionCode)); 
		return code;
	}
	
	/**
	 * Adds metadata to clojure expression
	 * @param cljExpr expression
	 * @param metadata collection of key - value metadata pairs
	 * @return Clojure code
	 */
	public static String addMetadata(String cljExpr, Collection<Pair<String, String>> metadata) {
		String code = ClojureHelper.applyClojureFunction(
				"vary-meta",
				cljExpr,
				"merge",
				mapHelper(metadata));
		
		return code;
	}
	
	/**
	 * Adds metadata to clojure expression
	 * 
	 * @param cljExpr expression
	 * @param bindings key - value metadata pairs
	 * @return Clojure code
	 */
	@SafeVarargs
	public static String addMetadata(String cljExpr, Pair<String, String> ...bindings) {
		return addMetadata(cljExpr, Arrays.asList(bindings));
	}
	
	/**
	 * Creates code for clojure set
	 * 
	 * @param members clojure code of the set members
	 * @return clojure code
	 */
	public static String clojureSetHelper(Collection<String> members) {
		StringBuilder sb = new StringBuilder("#{");
		
		Iterator<String> i = members.iterator();
		while(i.hasNext()) {
			String current = i.next();
			sb.append(current);
			if(i.hasNext()) {
				sb.append(" ");
			}
		}
		sb.append("}");
		return sb.toString();
	}
	
	/**
	 * Creates code for clojure set
	 * 
	 * @param members clojure code of the set members
	 * @return clojure code
	 */
	public static String clojureSetHelper(String ...members) {
		String code = clojureSetHelper(Arrays.asList(members));
		return code;
	}
	
	/**
	 * Creates code that instantiates java class in clojure.
	 * 
	 * @param instantiated instantiated class
	 * @param arguments arguments of the constructor
	 * @return Clojure code
	 */
	public static String instantiateJavaClass(Class<? extends Object> instantiated, Collection<String> arguments) {
		String code = applyClojureFunction(
				instantiated.getName() + ".",
				arguments);
		return code;
	}
	
	/**
	 * Creates code that instantiates java class in clojure.
	 * 
	 * @param instantiated instantiated class
	 * @param arguments arguments of the constructor
	 * @return Clojure code
	 */
	public static String instantiateJavaClass(Class<? extends Object> instantiated, String ...arguments) {
		String code = instantiateJavaClass(instantiated, Arrays.asList(arguments));
		return code;
	}
	
	/**
	 * Creares clojure code that checks if given expression is instance of given class name
	 * @param expression inspected expression
	 * @param className name of the class
	 * @return Clojure Code
	 */
	public static String isInstanceOfClass(String expression, String className) {
		return applyClojureFunction("instance?", className, expression);
	}
	
	/**
	 * Creares clojure code that checks if given expression is instance of given class name
	 * @param expression inspected expression
	 * @param cl class object
	 * @return Clojure code
	 */
	public static String isInstanceOfClass(String expression, Class<? extends Object> cl) {
		return isInstanceOfClass(expression, cl.getName());
	}
	
	/**
	 * Creates code for clojure try catch block
	 * @param tryClause code for try block
	 * @param exceptionClass class of catched exception
	 * @param exceptionName symbol to bind exception to
	 * @param catchClause code to execute in catch block
	 * @return clojure code
	 */
	public static String tryCatchHelper(String tryClause, Class<? extends Exception> exceptionClass, String exceptionName, String catchClause) {
		String code = applyClojureFunction(
				"try",
				tryClause,
				applyClojureFunction(
						"catch",
						exceptionClass.getName(),
						exceptionName,
						catchClause));
		return code;
	}
}
