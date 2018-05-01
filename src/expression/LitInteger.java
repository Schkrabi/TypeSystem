package expression;

import types.Type;
import types.TypeConcrete;
import interpretation.Environment;

/**
 * Abstract class for Integer Literals representations
 * @author Mgr. Radomir Skrabal
 *
 */
public class LitInteger extends Literal {
	
	public final int value;
	
	public LitInteger(int value){
		this.value = value;
	}

	@Override
	public Expression interpret(Environment env) {
		return this;
	}
	
	public static LitInteger initializeDefaultImplementation(int value){
		LitInteger l = new LitInteger(value);;
		l.setLiteralType(TypeConcrete.TypeInt); //TODO Deprecate?
		return l;
	}
	
	@Override
	public Type getDefaultRepresentationType() {
		return TypeConcrete.TypeInt;
	}

	@Override
	public String toClojureCode() throws Exception {
		return Integer.toString(this.value);
	}
	
	@Override
	public String toString(){
		return Integer.toString(this.value);
	}

	@Override
	public Type infer(Environment env) throws Exception {
		this.setType(TypeConcrete.TypeInt);
		return TypeConcrete.TypeInt;
	} 
}
