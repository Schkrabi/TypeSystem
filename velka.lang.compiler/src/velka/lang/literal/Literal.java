package velka.lang.literal;

import velka.lang.expression.Expression;
import velka.lang.interpretation.ClojureCodeGenerator;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;

/**
 * Abstract expression class for literals implementations
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Literal extends Expression {

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> p = this.infer(env, typeEnv);
		return clojureValueToClojureLiteral(this.valueToClojure(env, typeEnv), p.first);
	}

	/**
	 * Compiles value of this literal into clojure
	 * 
	 * @return String with clojure code
	 * @throws AppendableException if anything goes wrong during compilation
	 */
	protected abstract String valueToClojure(Environment env, TypeEnvironment typeEnv) throws AppendableException;
	
	/**
	 * Creates code for literal with metadata type in clojure
	 * @param clojureValue clojure code providing value for the literal
	 * @param type type of the literal
	 * @return clojure code
	 */
	public static String clojureValueToClojureLiteral(String clojureValue, Type type) {
		return ClojureCodeGenerator.addTypeMetaInfo("[" + clojureValue + "]", type);
	}
}
