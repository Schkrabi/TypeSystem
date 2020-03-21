package literal;

import expression.Expression;
import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeAtom;
import util.AppendableException;
import util.Pair;

/**
 * Literal for representing composed type literals
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class LitComposite extends Literal {

	/**
	 * Composed values of this literal
	 */
	public final Expression value;
	/**
	 * Type of this literal
	 */
	public final TypeAtom composedType;

	public LitComposite(Expression value, TypeAtom composedType) {
		super();
		this.value = value;
		this.composedType = composedType;
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		return new LitComposite(this.value.interpret(env), this.composedType);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) {
		return new Pair<Type, Substitution>(this.composedType, Substitution.EMPTY);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		return this.value.toClojureCode(env);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof LitComposite) {
			return this.composedType.equals(((LitComposite) other).composedType)
					&& this.value.equals(((LitComposite) other).value);
		}
		return false;
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof LitComposite) {
			int cmp = this.composedType.compareTo(((LitComposite) other).composedType);
			if (cmp != 0)
				return cmp;
			return this.value.compareTo(((LitComposite) other).value);
		}
		return super.compareTo(other);
	}

	@Override
	public int hashCode() {
		return this.value.hashCode() * this.composedType.hashCode();
	}

	@Override
	public String toString() {
		return this.value.toString();

		// return "<" + this.composedType.toString() + " "
		// + this.value.stream().map(x -> x.toString() + ",").reduce("", (x, y) -> x +
		// y) + ">";
	}
}
