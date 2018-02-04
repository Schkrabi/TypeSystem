package expression;

import interpretation.Environment;
import types.Type;
import types.TypeConcrete;

/**
 * Class for string literals
 * @author Mgr. Radomir Skrabal
 *
 */
public class LitString extends Literal {
	/**
	 * value of the string literal
	 */
	public final String value;
	
	public LitString(String value) {
		this.value = value;
		this.setType(TypeConcrete.TypeString);
	}

	@Override
	public Type getDefaultRepresentationType() {
		return TypeConcrete.TypeString;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		return this;
	}
	
	@Override
	public String toString() {
		return "\"" + this.value + "\"";
	}

	@Override
	public String toClojureCode() throws Exception {
		return '"' + this.value + '"';
	}
}
