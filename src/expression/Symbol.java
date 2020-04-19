package expression;

import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;
import abstraction.Operator;
import interpretation.Environment;

/**
 * Variable expression
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Symbol extends Expression implements Comparable<Expression> {

	/**
	 * Name of the variable
	 */
	public final String name;

	public Symbol(String name) {
		this.name = name;
	}

	@Override
	public int compareTo(Expression o) {
		if (o instanceof Symbol) {
			Symbol other = (Symbol) o;
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
	public String toClojureCode(Environment env) throws AppendableException {
		if (env.containsVariable(this)) {
			Expression e = env.getVariableValue(this);
			if (e instanceof Operator) {
				return e.toClojureCode(env);
			}
		}

		return this.name;
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Symbol) {
			return this.name.equals(((Symbol) other).name);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.name.hashCode();
	}

	@Override
	public TypeArrow getFunctionTypeWithRepresentations(TypeTuple argTypes, Environment env)
			throws AppendableException {
		if (env.containsVariable(this)) {
			return env.getVariableValue(this).getFunctionTypeWithRepresentations(argTypes, env);
		}
		return super.getFunctionTypeWithRepresentations(argTypes, env);
	}
}