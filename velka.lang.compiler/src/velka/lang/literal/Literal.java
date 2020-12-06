package velka.lang.literal;

import velka.lang.expression.Expression;
import velka.lang.interpretation.Environment;
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
	public String toClojureCode(Environment env) throws AppendableException {
		Pair<Type, Substitution> p = this.infer(env);
		return "(with-meta [" + this.valueToClojure(env) + "] {:lang-type " + p.first.clojureTypeRepresentation()
				+ "})";
	}

	/**
	 * Compiles value of this literal into clojure
	 * 
	 * @return String with clojure code
	 * @throws AppendableException if anything goes wrong during compilation
	 */
	protected abstract String valueToClojure(Environment env) throws AppendableException;
}
