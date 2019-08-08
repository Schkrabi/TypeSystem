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
public final class TypeHolder extends Expression implements Comparable<Expression> {
	
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
	public Expression interpret(Environment env) throws AppendableException {
		throw new AppendableException(this.getClass().getName() + " is not to be interpreted!");
	}

	/* (non-Javadoc)
	 * @see expression.Expression#infer(interpretation.Environment)
	 */
	@Override
	public Pair<Type, Substitution> infer(Environment env) {
		return new Pair<Type, Substitution>(this.type, new Substitution());
	}

	/* (non-Javadoc)
	 * @see expression.Expression#toClojureCode()
	 */
	@Override
	public String toClojureCode() throws AppendableException {
		throw new AppendableException(this.getClass().getName() + " is not to be converted to Clojure!");
	}

	@Override
	public boolean equals(Object other) {
		if(other instanceof TypeHolder) {
			return this.type.equals(((TypeHolder) other).type);
		}
		return false;
	}
	
	@Override
	public int compareTo(Expression o) {
		if(o instanceof TypeHolder) {
			return this.type.compareTo(((TypeHolder) o).type);
		}
		return super.compareTo(o);
	}
	
	@Override
	public String toString() {
		return "E:" + this.type.toString();
	}
}
