package expression;

import types.Type;
import types.TypeConcrete;
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
		this.setType(TypeConcrete.TypeDouble);
	}

	@Override
	public Type getDefaultRepresentationType() {
		return TypeConcrete.TypeDouble;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		return this;
	}

	@Override
	public String toString() {
		return Double.toString(this.value);
	}

	@Override
	public String toClojureCode() throws Exception {
		return Double.toString(this.value);
	}

	@Override
	public Type infer(Environment env) throws Exception {
		this.setType(TypeConcrete.TypeDouble);
		return TypeConcrete.TypeDouble;
	}
}
