package velka.util;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
	 * @return string with code
	 */
	public static String applyVelkaFunction_argsTuple(String funCode, String argsTuple) {
		String code = applyClojureFunction(
				ClojureCoreSymbols.eapplyClojureSymbol_full,
				funCode,
				argsTuple);
		return code;
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
		return applyClojureFunction("if", conditionCode, trueCode, falseCode);
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
	
//	/**
//	 * Gets inner value of literal
//	 * @param literalCode
//	 * @return
//	 */
//	public static String getLiteralInnerValue(String literalCode) {
//		return ClojureHelper.applyClojureFunction("first", literalCode);
//	}
	
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
	
	public static String setCostAndType(String fnCode, String costFunctionCode, String typeInfo) {
		String code = addMetadata(fnCode, 
				new Pair<String, String>(ClojureCoreSymbols.costFunctionKey, costFunctionCode),
				new Pair<String, String>(":lang-type", typeInfo)); 
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
	
	/**
	 * Class to hold method implementation for CloureHelper.proxy method
	 * @author Mgr. Radomir Skrabal
	 *
	 */
	public static class ProxyImpl {
		private final Method method;
		private final List<String> args;
		private final String implCode;
		
		private ProxyImpl(Method method, Collection<String> args, String implCode) {
			this.method = method;
			this.args = new ArrayList<String>(args);
			this.implCode = implCode;
		}
		
		/**
		 * Creates clojure code of this implementation
		 * @return clojure code
		 */
		public String toClojureCode() {
			return (new StringBuilder())
					.append("(")
					.append(this.method.getName())
					.append(" ")
					.append(clojureVectorHelper(this.args))
					.append(" ")
					.append(implCode)
					.append(")")
					.toString();
		}
		
		/**
		 * Factory constructor
		 * @param method implemented method
		 * @param args argument names of implemented method
		 * @param implCode code of implemented method
		 * @return ProxyImpl instance
		 */
		public static ProxyImpl of(Method method, Collection<String> args, String implCode) {
			return new ProxyImpl(method, args, implCode);
		}
	}
	
	/**
	 * Creates application of proxy special form implementing java subclasses in clojure
	 * @param clazz implemented class
	 * @param superArgs args for super constructor
	 * @param impls implementations
	 * @return Clojure code
	 */
	public static String proxy(Class<?> clazz, Collection<String> superArgs, ProxyImpl ...impls) {
		return
		applyClojureFunction(
				"proxy",
				clojureVectorHelper(Arrays.asList(clazz.getName())),
				clojureVectorHelper(superArgs),
				Arrays.stream(impls).map(i -> i.toClojureCode()).reduce((x, y) -> x + " " + y).get());
	
	}
	
	/**
	 * Creates clojure code to construct java class with given args
	 * @param clazz constructed class
	 * @param args arguments
	 * @return clojure code
	 */
	public static String constructJavaClass(Class<?> clazz, Collection<String> args) {
		return 
			applyClojureFunction(
					clazz.getName() + ".",
					args);
	}
	
	/**
	 * Creates clojure code to construct java class with given args
	 * @param clazz constructed class
	 * @param args arguments
	 * @return clojure code
	 */
	public static String constructJavaClass(Class<?> clazz, String ...args) {
		return constructJavaClass(clazz, Arrays.asList(args));
	}
	
	/** Wraps operator in fn of the same arity, used for defining operators with metadata */
	public static String wrapClojureOperatorToFn(int arity, String clojureOperator) {
		var args = Stream.iterate(0, x -> x + 1).limit(arity).map(x -> "_" + Integer.toString(x)).collect(Collectors.toList());
		String code =
				ClojureHelper.fnHelper(args, ClojureHelper.applyClojureFunction(clojureOperator, args));
		return code;
	}
	
	public static String unaryOperatorToFn(String clojureOperator) {
		return wrapClojureOperatorToFn(1, clojureOperator);
	}
	
	public static String binaryOperatorToFn(String clojureOperator) {
		return wrapClojureOperatorToFn(2, clojureOperator);
	}
	
	/** Creates clojure implementation of java interface */
	public static String reify(Class<?> clazz, Collection<Pair<String, Pair<Collection<String>, String>>> implementations) {
		var sb = new StringBuilder();
		
		for(var fun : implementations) {
			var s = applyClojureFunction(
					fun.first, 
					clojureVectorHelper(fun.second.first),
					fun.second.second);
			sb.append(s);
		}
		
		return applyClojureFunction(
				"reify", 
				clazz.getName(),
				sb.toString());
	}
	
	/** Creates clojure implementatio of java inteface*/
	@SafeVarargs
	public static String reify(Class<?> clazz, Pair<String, Pair<Collection<String>, String>> ...impls) {
		return reify(clazz, List.of(impls));
	}
	
	/** Instantiates java.util.funtion.Function in clojure */
	public static String javaFunIntfToClj(String arg, String body) {
		final var rhis = "_this";
		var code = reify(java.util.function.Function.class,
				Pair.of("apply", Pair.of(List.of(rhis, arg), body)));
		return code;
	}
	
	/** Instantiates java.util.function.Consumer in clojure */
	public static String javaConsumerToClj(String arg, String body) {
		final var rhis = "_this";
		var code = reify(java.util.function.Consumer.class,
				Pair.of("accept", Pair.of(List.of(rhis, arg), body)));
		return code;
	}
	
	/** Instantiates java.util.function.BiFunction in clojure */
	public static String javaBiFunIntToClj(String arg1, String arg2, String body) {
		final var rhis = "_this";
		var code = reify(java.util.function.BiFunction.class,
				Pair.of("apply", Pair.of(List.of(rhis, arg1, arg2), body)));
		return code;
	}
	
	/** Creates a def expression */
	public static String def(String symbol, String value) {
		return applyClojureFunction("def", symbol, value);
	}
	
	/** Standard clj symbol for command line args */
	public static final String COMMAND_LINE_ARGS = "*command-line-args*";
	
	/** Varargs syntax for clojure */
	public static final String varargs(String symbol) {
		return new StringBuilder()
				.append("& ")
				.append(symbol)
				.toString();
	}
}
