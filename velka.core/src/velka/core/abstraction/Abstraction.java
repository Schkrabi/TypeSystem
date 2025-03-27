package velka.core.abstraction;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.sun.codemodel.JVar;
import com.sun.codemodel.JExpr;
import com.sun.codemodel.JMethod;

import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.java.CodeModelInstance;
import velka.java.TypeUtil;
import velka.java.runtime.JavaTypeSystem;
import velka.java.runtime.VelkaTuple;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeTuple;
import velka.types.typeSystem.VelkaAbstraction;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.Pair;

/**
 * Parent class for all abstractions (lambdas, extended lambdas, functions,
 * extended functions and operators)
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Abstraction extends Expression implements VelkaAbstraction {

	protected abstract Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException;

	public Expression substituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
		return this.doSubstituteAndEvaluate(args, env);
	}

	/**
	 * Creates lexical clojure for this abstraction.
	 * Runtime interpretation method
	 * 
	 * @param formalArgs      formal arguments (argument variables)
	 * @param realArgs        arguments
	 * @param baseEnvironment environment where body of abstraction will be
	 *                        evaluated, also parent environment of lexical clojure
	 * @return Environment that is lexical clojure with given formal and real
	 *         arguments
	 * @throws AppendableException
	 */
	protected static Environment lexicalClojure(Tuple formalArgs, Tuple realArgs, Environment baseEnvironment)
			throws AppendableException {
		Environment ret = Environment.create(baseEnvironment);
		Iterator<Expression> i = formalArgs.iterator();
		Iterator<Expression> j = realArgs.iterator();

		while (i.hasNext()) {
			Expression e = j.next();
			Symbol v = (Symbol) i.next();
			ret.put(v, e);
		}

		return ret;
	}
	
	/**
	 * Creates clojure code for implementations of this abstraction
	 * @param env environemnt where evaluation takes place
	 * @return String containing clojure code
	 * @throws AppendableException
	 */
	protected abstract String implementationsToClojure(Environment env) throws AppendableException;
	
	@Override
	public String toClojureCode(Environment env) throws AppendableException{
		Pair<Type, Substitution> p = this.infer(env);
		String code = Type.addTypeMetaInfo(this.implementationsToClojure(env), p.first);
		
		return code;
	}
	
	/**
	 * Selects implementation for this abstraction based on arguments and ranking function
	 * Runtime interpretation method
	 * 
	 * @param args arguments of application
	 * @param rankingFunction ranking function
	 * @return abstraction
	 * @throws AppendableException 
	 */
	public abstract Abstraction selectImplementation(Tuple args, Environment env) throws AppendableException;
	
	/**
	 * Does the inference with specified arguments
	 * Arguments must not be evaluated, only infered. Typeholders should be possible.
	 * @param args arguments with which abstraction is applied
	 * @param env environment
	 * @param typeEnv type environment
	 * @return pair of infered type and used substitution
	 * @throws AppendableException 
	 */
	public abstract Pair<Type, Substitution> inferWithArgs(Tuple args, Environment env) throws AppendableException;

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
	public static String makeLambdaDef(String fnName, Abstraction fn, Environment env)
			throws AppendableException {
		String code =  ClojureHelper.applyClojureFunction("def", fnName, fn.toClojureCode(env));
		
		return code;
	}
	
	/** Converts and declares the arguments in the method*/
	public static Map<Symbol, JVar> convertAndDeclareParms(Collection<Pair<Symbol, Type>> parms, JMethod method){
		var typeSystem = JavaTypeSystem.codeInstance();
		var parm = method.param(Collection.class, "_parm");
		
		var vtCl = CodeModelInstance.instance().ref(VelkaTuple.class);
		var cparm = method.body().decl(vtCl, "_cparm", 
				JExpr.cast(vtCl,
						typeSystem.invoke("convert")
						.arg(typeSystem.invoke("getType").arg(parm))
						.arg(TypeUtil.instance().type2java(new TypeTuple(parms.stream().map(x -> x.second).toList())))
						.arg(parm)
						.arg(JExpr._null())));
		
		var ret = new HashMap<Symbol, JVar>();
		int i = 0;
		
		for(var p : parms) {
			var jt = TypeUtil.instance().velkaTypeToJType(p.second);
			var v = method.body().decl(jt, p.first.getJavaCompatibleName(),
					JExpr.cast(jt, cparm.invoke("get").arg(JExpr.lit(i))));
			ret.put(p.first, v);
			i++;
		}
		
		return ret;
	}
	
	public static Map<Symbol, JVar> declareArgs(Collection<Pair<Symbol, Type>> parms, JMethod method, Environment env){
		var parm = method.param(Collection.class, "_parm");
		
		var cVelkaTuple = CodeModelInstance.instance().ref(VelkaTuple.class);
		
		var tparm = method.body().decl(cVelkaTuple, "_tparm",
				JExpr.cast(cVelkaTuple, parm));
		
		
		var ret = new HashMap<Symbol, JVar>();
		int i = 0;
		for(var p : parms){
			var sym = p.first;
			var type = p.second;
			var jt = TypeUtil.instance().velkaTypeToJType(type);
			var v = method.body().decl(jt, sym.getJavaCompatibleName(), 					
					JExpr.cast(jt, tparm.invoke("get").arg(JExpr.lit(i))));
			ret.put(sym, v);
			i++;
		}
		
		return ret;
	}
}
