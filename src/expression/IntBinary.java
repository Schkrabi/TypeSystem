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
		return TypeConcrete.TypeIntBinary;
	}

	@Override
	public Literal fromDefaultRepresentation(Literal l) {
		return l; //TODO ??
	}

	@Override
	public Literal toDefaultRepresentation() {
		return this;
	}
	

	@Override
	public Literal convertRepresentation(Class<? extends Literal> c) throws Exception {
		if(c == IntBinary.class){
			return this;
		}
		if(c == IntRoman.class){
			return new IntRoman(IntRoman.int2roman(this.value));
		}
		if(c == IntString.class){
			return new IntString(Integer.toString(this.value));
		}
		return super.convertRepresentation(c);
	}
}
