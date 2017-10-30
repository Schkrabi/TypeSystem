package expression;

import types.Type;
import types.TypeConcrete;
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
		this.setType(TypeConcrete.TypeBool);
		return TypeConcrete.TypeBool;
	}

	@Override
	public Type getDefaultRepresentationType() {
		return TypeConcrete.TypeBool;
	}

	@Override
	public Literal fromDefaultRepresentation(Literal l) {
		return l; // TODO ??
	}

	@Override
	public Literal toDefaultRepresentation() {
		return this;
	}

	@Override
	public Literal convertRepresentation(Class<? extends Literal> c) {
		// No non-default representations for Bool
		return this;
	}
}
