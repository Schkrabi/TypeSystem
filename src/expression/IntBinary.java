package expression;

import interpretation.Environment;
import types.Type;
import types.TypeConcrete;

public class IntBinary extends LitInteger {
	public final int value;
	
	public IntBinary(int value) {
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
