package expression;

import interpretation.Environment;
import types.Type;
import types.TypeConcrete;

public class Subtraction extends Lambda {
	
	public static final Subtraction singleton = new Subtraction();
	
	private Subtraction() {
		super(new Tuple(new Variable[]{new Variable("_x"), new Variable("_y")}), SubWrapper.singleton);
	}
	
	public String toString() {
		return "- _x _y";
	}
	
	private static class SubWrapper extends Expression {
		public static final SubWrapper singleton = new SubWrapper();
		
		private SubWrapper() {}

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
		public Type infer() throws Exception {
			return TypeConcrete.TypeInt;
		}
		
	}
}
