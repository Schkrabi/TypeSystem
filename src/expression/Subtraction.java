package expression;

import interpretation.Environment;
import types.Type;
import types.TypeArrow;
import types.TypeConcrete;
import types.TypeTuple;

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
			IntBinary x = (IntBinary) env.getVariableValue(new Variable("_x"));
			IntBinary y = (IntBinary) env.getVariableValue(new Variable("_y"));
			
			if(x == null || y == null) {
				return this;
			}
			
			return new IntBinary(x.value - y.value); //TODO HOTFIX
		}

		@Override
		public Type infer() throws Exception {
			this.setType(TypeConcrete.TypeIntBinary);
			return TypeConcrete.TypeIntBinary;
		}
		
	}
}
