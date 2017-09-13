package expression;

import interpretation.Environment;

public class Variable extends Expression implements Comparable<Variable> {
	
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
		if(!env.containsVariable(this)) {
			throw new Exception("Unbound variable");
		}
		return env.getVariableValue(this); //Lazy?
	}
	
	@Override
	public String toString() {
		return this.name;
	}

}
