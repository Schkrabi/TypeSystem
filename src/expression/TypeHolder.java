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
 *         This class is used as placeholder for variables creates in
 *         environments during type inference. It hsould be never interpreted
 *
 */
public final class TypeHolder extends Expression implements Comparable<Expression> {

	/**
	 * Type held and returned by inference on this expression
	 */
	public final Type type;

	/**
	 * Expression for which this typeholder is placed
	 */
	public final Variable placeholderOf;

	public TypeHolder(Type type) {
		this.type = type;
		this.placeholderOf = null;
	}

	public TypeHolder(Type type, Variable placeholderOf) {
		this.type = type;
		this.placeholderOf = placeholderOf;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see expression.Expression#interpret(interpretation.Environment)
	 */
	@Override
	public Expression interpret(Environment env) throws AppendableException {
		if (this.placeholderOf == null || !env.containsVariable(this.placeholderOf)) {
			throw new AppendableException(this.getClass().getName() + " is not to be interpreted!");
		}
		Expression e = env.getVariableValue(this.placeholderOf);
		if (e.equals(this)) {
			if (env.isTopLevel()) {
				throw new AppendableException(this.getClass().getName() + " is not to be interpreted!");
			}
			return this.placeholderOf.interpret(env.parent);
		}
		return e.interpret(env);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see expression.Expression#infer(interpretation.Environment)
	 */
	@Override
	public Pair<Type, Substitution> infer(Environment env) {
		return new Pair<Type, Substitution>(this.type, Substitution.EMPTY);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see expression.Expression#toClojureCode()
	 */
	@Override
	public String toClojureCode() throws AppendableException {
		return this.toClojureCode(null, Environment.topLevelEnvironment);
	}

	@Override
	protected String toClojureCode(Type expectedType, Environment env) throws AppendableException {
		throw new AppendableException(this.getClass().getName() + " is not to be converted to Clojure!");
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof TypeHolder) {
			return this.type.equals(((TypeHolder) other).type);
		}
		return false;
	}

	@Override
	public int compareTo(Expression o) {
		if (o instanceof TypeHolder) {
			return this.type.compareTo(((TypeHolder) o).type);
		}
		return super.compareTo(o);
	}

	@Override
	public String toString() {
		return "E:" + this.type.toString();
	}

	@Override
	public int hashCode() {
		return this.type.hashCode();
	}
}
