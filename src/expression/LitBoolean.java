package expression;

import types.Type;
import types.TypeConcrete;
import util.AppendableException;
import interpretation.Environment;

/**
 * Boolean literal implementation
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class LitBoolean extends Literal {

	/**
	 * Literal value
	 */
	public final boolean value;

	private LitBoolean(boolean value) { 
		this.value = value;
		this.setType(TypeConcrete.TypeBool);
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
	public Type getDefaultRepresentationType() {
		return TypeConcrete.TypeBool;
	}

	@Override
	public String toClojureCode() throws Exception {
		return Boolean.toString(this.value);
	}

	@Override
	public Type infer(Environment env) throws AppendableException {
		this.setType(TypeConcrete.TypeBool);
		return TypeConcrete.TypeBool;
	}
	
	@Override
	public boolean equals(Object o){
		if(!(o instanceof LitBoolean)){
			return false;
		}
		LitBoolean other = (LitBoolean) o;
		return this.value == other.value;
	}
}
