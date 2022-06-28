package velka.core.expression;

import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.NameGenerator;
import velka.util.Pair;
import velka.core.abstraction.Operator;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeVariable;

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
	
	/**
	 * Namespace of the symbol
	 */
	public final String namespace;

	public Symbol(String name) {
		this.name = name;
		this.namespace = "";
	}
	
	public Symbol(String name, String namespace) {
		this.name = name;
		this.namespace = namespace;
	}

	@Override
	public int compareTo(Expression o) {
		if (o instanceof Symbol) {
			Symbol other = (Symbol) o;
			int c = this.name.compareTo(other.name);
//			if(c != 0) {
//				return c;
//			}
//			c = this.namespace.compareTo(other.namespace);
			return c;
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
				return ((Operator) e).getClojureSymbol().toClojureCode(env, typeEnv);
			}
		}

		if(!this.namespace.isEmpty()) {
			return ClojureHelper.fullyQualifySymbol(this.namespace, this.name);
		}
		return this.name;
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Symbol) {
			return this.name.equals(((Symbol) other).name);// &&
					//this.namespace.equals(((Symbol) other).namespace);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.name.hashCode();// * this.namespace.hashCode();
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		Expression intprt = this.interpret(env, typeEnv);
		if(intprt.equals(this)) {
			return this;
		}
		return intprt.convert(to, env, typeEnv);
	}
}
