package expression;

import types.Type;
import types.TypeConcrete;
import interpretation.Environment;

public class Addition extends Lambda {
	
	public static final Addition singleton = new Addition();
	
	private Addition() {
		super(new Tuple(new Variable[]{new Variable("_x"), new Variable("_y")}), AddWrapper.singleton);
	}	
	
	@Override
	public String toString() {
		return "+ _x _y";
	}
	
	private static class AddWrapper extends Expression{
		public static final AddWrapper singleton = new AddWrapper();
		
		private AddWrapper(){}
		
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
		public Type infer() throws Exception {
			return TypeConcrete.TypeInt;
		}
	}
}
