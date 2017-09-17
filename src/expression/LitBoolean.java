package expression;

import types.Type;
import types.TypeConcrete;
import interpretation.Environment;

public class LitBoolean extends Literal {
	
	public final boolean value;
	
	private LitBoolean(boolean value) {
		this.value = value;
	}

	@Override
	public Expression interpret(Environment env) {
		return this;
	}
	
	@Override
	public String toString() {
		return Boolean.toString(this.value);	
	}
	
	public static final LitBoolean TRUE = new LitBoolean(true);
	public static final LitBoolean FALSE = new LitBoolean(false);

	@Override
	public Type infer() throws Exception {
		return TypeConcrete.TypeBool;
	}
}
