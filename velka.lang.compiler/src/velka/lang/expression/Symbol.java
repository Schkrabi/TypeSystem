package velka.lang.expression;

import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeVariable;
import velka.lang.util.AppendableException;
import velka.lang.util.NameGenerator;
import velka.lang.util.Pair;
import velka.lang.abstraction.Operator;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;

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
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		if (!env.containsVariable(this)) {
			return this;
		}
		return env.getVariableValue(this).interpret(env, typeEnv);
	}

	@Override
	public String toString() {
		return this.name;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		try {
			if (env.containsVariable(this)) {
				return env.getVariableValue(this).infer(env, typeEnv);
			}
			return new Pair<Type, Substitution>(new TypeVariable(NameGenerator.next()), Substitution.EMPTY);
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		if (env.containsVariable(this)) {
			Expression e = env.getVariableValue(this);
			if (e instanceof Operator) {
				return e.toClojureCode(env, typeEnv);
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
}
