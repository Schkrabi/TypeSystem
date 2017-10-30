package expression;

import types.Type;
import types.TypeVariable;
import util.NameGenerator;
import interpretation.Environment;

/**
 * Variable expression
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Variable extends Expression implements Comparable<Variable> {

	/**
	 * Name of the variable
	 */
	public final String name;

	public Variable(String name) {
		this.name = name;
	}

	@Override
	public int compareTo(Variable other) {
		return this.name.compareTo(other.name);
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		if (!env.containsVariable(this)) {
			throw new Exception("Unbound variable");
		}
		return env.getVariableValue(this); // Lazy?
	}

	@Override
	public String toString() {
		return this.name;
	}

	@Override
	public Type infer() throws Exception {
		Type t = new TypeVariable(NameGenerator.next());
		this.setType(t);
		return t;
	}

}
