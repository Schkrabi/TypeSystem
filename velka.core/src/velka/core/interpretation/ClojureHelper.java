package velka.core.interpretation;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import velka.core.abstraction.Abstraction;
import velka.core.abstraction.Operator;
import velka.types.Type;
import velka.util.AppendableException;
import velka.util.Pair;

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
	public static String addTypeMetaInfo(String cljCode, Type type) {
		try {
			return ClojureHelper.addTypeMetaInfo_str(cljCode, type.clojureTypeRepresentation());
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
		return "(declare " + symbol + ")\n";
	}
	
	public static String makeOperatorDeclaration(Operator operator) {
		return ClojureHelper.makeDeclaration(operator.getClojureSymbol().name);
	}

	/**
	 * Creates dynamic declaration for given symbol
	 * 
	 * @param symbol declared symbol
	 * @return string with code
	 */
	public static String makeDynamicDeclaration(String symbol) {
		return "(declare ^:dynamic " + symbol + ")\n";
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

	/**
	 * Creates definition for an abstraction in clojure
	 * 
	 * @param fnName name of the function
	 * @param fn defined abstraction
	 * @param env environment
	 * @param typeEnv type environment
	 * @return string with code
	 * @throws AppendableException
	 */
	public static String makeLambdaDef(String fnName, Abstraction fn, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		return "(def " + fnName + " " + fn.toClojureCode(env, typeEnv) + ")\n";
	}
	
	/**
	 * Creates definitions for operator 
	 * 
	 * @param operator defined operator
	 * @param env environment
	 * @param typeEnvtype environment
	 * @return string with code
	 * @throws AppendableException
	 */
	public static String makeOperatorDef(Operator operator, Environment env, TypeEnvironment typeEnv) 
		throws AppendableException {
		return makeLambdaDef(operator.getClojureSymbol().name, operator, env, typeEnv);
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
		final String tuple = "_tuple";
		
		return ClojureHelper.letHelper(
				ClojureHelper.addTypeMetaInfo_str(tuple, "(velka.types.TypeTuple. (map " + ClojureCoreSymbols.getTypeClojureSymbol_full + " " + tuple + "))"), 
				new Pair<String, String>(tuple, ClojureHelper.clojureVectorHelper(members)));
	}
	
	/**
	 * Applies velka function in clojure code
	 * @param funCode velka function code
	 * @param args arguments to apply with
	 * @return string with code
	 */
	public static String applyVelkaFunction(String funCode, String ...args)  {
		StringBuilder sb = new StringBuilder();
		
		sb.append("(");
		sb.append(ClojureCoreSymbols.eapplyClojureSymbol_full);
		sb.append(" \n");
		sb.append(funCode);
		sb.append(" \n");
		sb.append(ClojureHelper.tupleHelper(args));
		
		sb.append(")");
		
		return sb.toString();
	}
	
	/**
	 * Creates clojure if expression from arguments
	 * @param conditionCode condition
	 * @param trueCode true branch
	 * @param falseCode false branch
	 * @return string with code
	 */
	public static String ifHelper(String conditionCode, String trueCode, String falseCode) {
		return "(if (first " + conditionCode + ") " + trueCode + " " + falseCode + ")";
	}
	
	/**
	 * Creates clojure fn expression from arguments
	 * @param fnspecs specification of all arguments-body pairs for fn
	 * @return string with code
	 */
	public static String fnHelper(List<Pair<List<String>, String>> fnspecs) {
		StringBuilder sb = new StringBuilder("(fn ");
		
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
	 * Creates code for composite literal (LitComposite) in clojure.
	 * @param type type of composite literal
	 * @param value value of the composite literal
	 * @return string with code
	 * @throws AppendableException if there is issue with compiling type into clojure
	 */
	public static String litCompositeHelper(Type type, String value) throws AppendableException {
		return litCompositeHelper_str(type.clojureTypeRepresentation(), value);
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
}
