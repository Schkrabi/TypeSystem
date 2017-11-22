package types;

import expression.Expression;
import expression.Literal;
import interpretation.Environment;

/**
 * Class for floating point number literal
 * @author Mgr. Radomir Skrabal
 *
 */
public class LitDouble extends Literal {
	
	/**
	 * Value of the floating point literal
	 */
	public final double value;
	
	public LitDouble(double value) {
		this.value = value;
	}

	@Override
	public Type getDefaultRepresentationType() {
		return TypeConcrete.TypeDouble;
	}

	@Override
	public Literal fromDefaultRepresentation(Literal l) {
		return this;
	}

	@Override
	public Literal toDefaultRepresentation() throws Exception {
		return this;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		return this;
	}

	@Override
	public Type infer() throws Exception {
		return TypeConcrete.TypeDouble;
	}

	@Override
	public String toString() {
		return Double.toString(this.value);
	}
}
