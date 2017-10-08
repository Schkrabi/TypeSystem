package expression;

import types.Type;
import types.TypeConcrete;
import interpretation.Environment;

public abstract class LitInteger extends Literal {

	@Override
	public Expression interpret(Environment env) {
		return this;
	}

	@Override
	public Type infer() throws Exception {
		return TypeConcrete.TypeInt;
	}
	
	public static LitInteger initializeDefaultImplementation(int value){
		return new IntBinary(value);
	}
}
