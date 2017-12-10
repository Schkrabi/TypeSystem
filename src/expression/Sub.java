package expression;

import types.Type;
import interpretation.Environment;

/** @deprecated */
public class Sub extends Expression {
	
	private Sub() {}
	
	public static final Sub singleton = new Sub();

	@Override
	public Expression interpret(Environment env) throws Exception {
		IntBinary x = (IntBinary) env.getVariableValue(new Variable("_x"));
		IntBinary y = (IntBinary) env.getVariableValue(new Variable("_y"));
		
		if(x == null || y == null) {
			return this;
		}
		
		return new IntBinary(x.value - y.value);
	}
	
	@Override
	public String toString() {
		return "- _x _y";
	}

	@Override
	public Type infer() throws Exception {
		throw new Exception("Trying to infer a function wrapper!");
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		return this;
	}

	@Override
	public String toClojureCode() throws Exception {
		throw new Exception("Obsolete Sub used!");
	}
}
