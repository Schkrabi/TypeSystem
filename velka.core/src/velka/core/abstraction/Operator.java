package velka.core.abstraction;

import java.util.Optional;

import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.ClojureHelper;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Substitution;
import velka.types.Type;
import velka.util.AppendableException;
import velka.util.Pair;

/**
 * Expression for meta-language operators
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Operator extends Abstraction {

	/**
	 * Creates clojure function for the operator
	 * @param env environment
	 * @param typeEnv type environment
	 * @return clojure code
	 * @throws AppendableException
	 */
	protected abstract String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException;
	
	/**
	 * Symbol used for operator in clojure
	 * @return fully qualified symbol
	 */
	public abstract Symbol getClojureSymbol();
	
	/**
	 * Makes code for defining conversion in clojure header
	 * @return code
	 */
	public String clojureDef() {
		Environment env = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv;
		try {
			typeEnv = TypeEnvironment.initBasicTypes(env);
			return this.toClojureCode(env, typeEnv);
		} catch (AppendableException e) {
			e.printStackTrace();
		}
		return "";
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		return this;
	}

	@Override
	public Abstraction selectImplementation(Tuple args, Optional<Expression> rankingFunction, Environment env,
			TypeEnvironment typeEnv) throws AppendableException {
		return this;
	}

	@Override
	protected String implementationsToClojure(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> p = this.infer(env, typeEnv);
		StringBuilder sb = new StringBuilder();

		sb.append("(let [impl ");
		sb.append(ClojureHelper.addTypeMetaInfo(this.toClojureOperator(env, typeEnv), p.first));
		sb.append("] \n");
		sb.append("(fn ");
		sb.append("([args] impl) \n");
		sb.append("([args ranking-fn] impl)))");

		return ClojureHelper.addTypeMetaInfo(sb.toString(), p.first);
	}
	
	@Override
	public Pair<Type, Substitution> inferWithArgs(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException{
		return this.infer(env, typeEnv);
	}
}
