package expression;

import types.Type;
import interpretation.Environment;

public class Sub extends Expression {
	
	private Sub() {}
	
	public static final Sub singleton = new Sub();

	@Override
	public Expression interpret(Environment env) throws Exception {
		LitInteger x = (LitInteger) env.getVariableValue(new Variable("_x"));
		LitInteger y = (LitInteger) env.getVariableValue(new Variable("_y"));
		
		if(x == null || y == null) {
			return this;
		}
		
		return new LitInteger(x.value - y.value);
	}
	
	@Override
	public String toString() {
		return "- _x _y";
	}

	@Override
	public Type infer() throws Exception {
		throw new Exception("Trying to infer a function wrapper!");
	}
}
