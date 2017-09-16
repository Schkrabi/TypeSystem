package expression;

import interpretation.Environment;

public class Add extends Expression {
	
	private Add() {}
	
	public static final Add singleton = new Add();

	@Override
	public Expression interpret(Environment env) throws Exception {
		LitInteger x = (LitInteger) env.getVariableValue(new Variable("_x"));
		LitInteger y = (LitInteger) env.getVariableValue(new Variable("_y"));
		
		if(x == null || y == null) {
			return this;
		}
		
		return new LitInteger(x.value + y.value);
	}
	
	@Override
	public String toString() {
		return "+ _x _y";
	}

}
