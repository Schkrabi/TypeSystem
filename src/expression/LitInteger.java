package expression;

import interpretation.Environment;

public abstract class LitInteger extends Literal {

	@Override
	public Expression interpret(Environment env) {
		return this;
	}
	
	public static LitInteger initializeDefaultImplementation(int value){
		return new IntBinary(value);
	}
}
