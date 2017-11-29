package expression;

import types.Type;
import types.TypeConcrete;
import interpretation.Environment;

/**
 * Abstract class for Integer Literals representations
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class LitInteger extends Literal {

	@Override
	public Expression interpret(Environment env) {
		return this;
	}
	
	public static LitInteger initializeDefaultImplementation(int value){
		return new IntBinary(value);
	}
	
	@Override
	public Type getDefaultRepresentationType() {
		return TypeConcrete.TypeInt;
	}
}
