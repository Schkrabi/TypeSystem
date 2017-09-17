package expression;

import types.Type;
import types.TypeConcrete;
import interpretation.Environment;

public class LitInteger extends Literal {
	
	public final int value;
	
	public LitInteger(int value) {
		this.value = value;
	}

	@Override
	public Expression interpret(Environment env) {
		return this;
	}
	
	@Override
	public String toString() {
		return Integer.toString(this.value);
	}

	@Override
	public Type infer() throws Exception {
		return TypeConcrete.TypeInt;
	}
}
