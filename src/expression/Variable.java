package expression;

import types.Substitution;
import types.Type;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;
import interpretation.Environment;
import operators.Operator;

/**
 * Variable expression
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Variable extends Expression implements Comparable<Expression> {

	/**
	 * Name of the variable
	 */
	public final String name;

	public Variable(String name) {
		this.name = name;
	}

	@Override
	public int compareTo(Expression o) {
		if (o instanceof Variable) {
			Variable other = (Variable) o;
			return this.name.compareTo(other.name);
		}
		return super.compareTo(o);
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		if (!env.containsVariable(this)) {
			return this;
		}
		return env.getVariableValue(this).interpret(env);
	}

	@Override
	public String toString() {
		return this.name;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		try {
			if (env.containsVariable(this)) {
				return env.getVariableValue(this).infer(env);
			}
			return new Pair<Type, Substitution>(new TypeVariable(NameGenerator.next()), Substitution.EMPTY);
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public String toClojureCode() throws AppendableException {
		return this.toClojureCode(null, Environment.topLevelEnvironment);
	}
	
	@Override
	protected String toClojureCode(Type expectedType, Environment env) throws AppendableException {
		if(env.containsVariable(this)) {
			Expression e = env.getVariableValue(this);
			if(e instanceof Operator) {
				return e.toClojureCode(expectedType, env);
			}
		}
		
		return this.name;
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Variable) {
			return this.name.equals(((Variable) other).name);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.name.hashCode();
	}
}
