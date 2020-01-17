package expression;

import util.AppendableException;
import util.InvalidClojureCompilationException;
import interpretation.Environment;
import types.Type;
import types.TypeTuple;

/**
 * Expression for representing interpreted functions and Extended functions
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class MetaFunction extends Expression {
	/**
	 * Environment where the function was created
	 */
	public final Environment creationEnvironment;

	public MetaFunction(Environment creationEnvironment) {
		this.creationEnvironment = creationEnvironment;
	}

	/**
	 * Returns implementation according to the comparator (if there are any
	 * alternative implementations)
	 * 
	 * @param c comparator
	 * @return function
	 */
	public abstract Function getFunction(TypeTuple realArgsType);

	@Override
	public String toClojureCode() throws InvalidClojureCompilationException, AppendableException {
		return this.toClojureCode(null, Environment.topLevelEnvironment);
	}

	@Override
	public String toClojureCode(Type expectedType, Environment env) throws InvalidClojureCompilationException, AppendableException {
		throw new InvalidClojureCompilationException(this);
	}

	@Override
	public Expression interpret(Environment env) {
		return this;
	}

	/**
	 * Returns true if given expression is a function
	 * 
	 * @param e
	 * @return
	 */
	public static boolean isFunction(Expression e) {
		return e instanceof MetaFunction;
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof MetaFunction) {
			return this.creationEnvironment.equals(((MetaFunction) other).creationEnvironment);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.creationEnvironment.hashCode();
	}
}
