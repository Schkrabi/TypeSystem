package expression;

import types.Substitution;
import types.Type;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;

import interpretation.Environment;

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
		if(o instanceof Variable) {
			Variable other = (Variable)o;
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
			Expression e = env.getVariableValue(this);
			if(e == null) {
				return new Pair<Type, Substitution>(new TypeVariable(NameGenerator.next()), new Substitution());
			}
			
			return e.infer(env);			
		}catch(AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public String toClojureCode() {
		return this.name;
	}
	
	@Override
	public boolean equals(Object other) {
		if(other instanceof Variable) {
			return this.name.equals(((Variable) other).name);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return this.name.hashCode();
	}
}
