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
		return env.getVariableValue(this).interpret(env);
	}

	@Override
	public String toString() {
		return this.name;
	}

	@Override
	public Type infer() throws Exception {
		if(this.inferedType != null){
			return this.getType();
		}
		Type t = new TypeVariable(NameGenerator.next());
		this.setType(t);
		return t;
	}

	public void setType(Type type) {
		this.inferedType = type;
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		Expression e = topLevel.getVariableValue(this);
		if(e != null){
			return e;
		}
		return this;
	}

	@Override
	public String toClojureCode() throws Exception {
		return this.name;
	}
}
