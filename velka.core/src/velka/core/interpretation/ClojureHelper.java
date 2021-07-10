package velka.core.interpretation;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

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
	
	public static String tupleHelper(String ...args) {
		return tupleHelper_array(args);
	}
	
	public static String tupleHelper_array(String[] members) {
		return tupleHelper(Arrays.asList(members));
	}
	
	public static String tupleHelper(Collection<String> members) {
		StringBuilder sb = new StringBuilder();
		
		sb.append("(let [tuple [");
		
		Iterator<String> i = members.iterator();
		while(i.hasNext())
		{
			String member = i.next();
			sb.append(member);
			if(i.hasNext()) {
				sb.append(" ");
			}
		}
		
		sb.append("]] ");
		sb.append(ClojureHelper.addTypeMetaInfo_str("tuple", "(velka.types.TypeTuple. (map " + ClojureCoreSymbols.getTypeClojureSymbol_full + " tuple))"));
		sb.append(")");
		
		return sb.toString();
	}
	
	public static String applyVelkaFunction(String funCode, String ...args) throws AppendableException {
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
	
	public static String ifHelper(String conditionCode, String trueCode, String falseCode) {
		return "(if (first " + conditionCode + ") " + trueCode + " " + falseCode + ")";
	}
	
	public static String fnHelper(List<String> args, String body) {
		StringBuilder sb = new StringBuilder("(fn [");
		
		Iterator<String> i = args.iterator();
		while(i.hasNext()) {
			String arg = i.next();
			sb.append(arg);
			if(i.hasNext()) {
				sb.append(" ");
			}
		}
		sb.append("] ");
		sb.append(body);
		sb.append(")");
		
		return sb.toString();
	}
	
	public static String litCompositeHelper_str(String type, String value) {
		return addTypeMetaInfo_str("[" + value + "]", type);
	}
	
	public static String litCompositeHelper(Type type, String value) throws AppendableException {
		return litCompositeHelper_str(type.clojureTypeRepresentation(), value);
	}
	
	public static String errorHelper(String code) {
		return "(throw (Throwable. " + code + "))";
	}
	
	public static String stringHelper(String str) {
		return "\"" + str + "\"";
	}
	
	public static class LetfnTriplet {
		public final String name;
		public final List<String> args;
		public final String body;
		
		public LetfnTriplet(String name, List<String> args, String body) {
			this.name = name;
			this.args = args;
			this.body = body;
		}
		
		public String toFnspec() {
			StringBuilder sb = new StringBuilder("(");
			sb.append(this.name);
			sb.append(" [");
			
			Iterator<String> i = this.args.iterator();
			while(i.hasNext()) {
				String arg = i.next();
				sb.append(arg);
				if(i.hasNext()) {
					sb.append(" ");
				}
			}
			sb.append("] ");
			sb.append(this.body);
			sb.append(")");
			return sb.toString();
		}
	}
	
	public static LetfnTriplet makeLetfnTriplet(String name, List<String> args, String body) {
		return new LetfnTriplet(name, args, body);
	}
	
	public static String letfnHelper(List<LetfnTriplet> fnspecs, String body) {
		StringBuilder sb = new StringBuilder("(letfn [");
		
		Iterator<LetfnTriplet> i = fnspecs.iterator();
		while(i.hasNext()) {
			LetfnTriplet fnspec = i.next();
			sb.append(fnspec.toFnspec());
			if(i.hasNext()) {
				sb.append("\n");
			}
		}
		sb.append("] ");
		sb.append(body);
		sb.append(")");
		return sb.toString();
	}
	
	public static String letfnHelper(String body, LetfnTriplet ...fnspecs) {
		return letfnHelper(Arrays.asList(fnspecs), body);
	}
	
	public static String letHelper(String body, List<Pair<String, String>> defs) {
		StringBuilder sb = new StringBuilder("(let [");
		
		Iterator<Pair<String, String>> i = defs.iterator();
		while(i.hasNext()) {
			Pair<String, String> def = i.next();
			sb.append(def.first);
			sb.append(" ");
			sb.append(def.second);
			if(i.hasNext()) {
				sb.append("\n");
			}
		}
		sb.append("] ");
		sb.append(body);
		sb.append(")");
		return sb.toString();
	}
	
	@SafeVarargs
	public static String letHelper(String body, Pair<String, String> ...defs) {
		return letHelper(body, Arrays.asList(defs));
	}
	
	public static String lambdaHelper(String clojureFn) {
		StringBuilder sb = new StringBuilder();
		
		sb.append("(let [impl ");
		sb.append(clojureFn);
		sb.append("] ");
		sb.append("(fn ");
		sb.append("([args] impl) ");
		sb.append("([args ranking-fn] impl)))");
		
		return sb.toString();
	}
}
