/**
 * 
 */
package expression;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import util.AppendableException;
import util.Pair;

/**
 * @author Mgr. Radomir Skrabal
 * 
 * This class is used as placeholder for variables creates in environments during type inference. It hsould be never interpreted
 *
 */
public final class TypeHolder extends Expression {
	
	/**
	 * Type held and returned by inference on this expression
	 */
	public final Type type;
	
	public TypeHolder(Type type) {
		this.type = type;
	}

	/* (non-Javadoc)
	 * @see expression.Expression#interpret(interpretation.Environment)
	 */
	@Override
	public Expression interpret(Environment env) throws Exception {
		throw new AppendableException(this.getClass().getName() + " is not to be interpreted!");
	}

	/* (non-Javadoc)
	 * @see expression.Expression#infer(interpretation.Environment)
	 */
	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return new Pair<Type, Substitution>(this.type, new Substitution());
	}

	/* (non-Javadoc)
	 * @see expression.Expression#toClojureCode()
	 */
	@Override
	public String toClojureCode() throws Exception {
		throw new AppendableException(this.getClass().getName() + " is not to be converted to Clojure!");
	}

}
