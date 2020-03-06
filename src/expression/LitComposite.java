package expression;

import java.util.Iterator;

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
	public final Tuple value;
	/**
	 * Type of this literal
	 */
	public final TypeAtom composedType;

	public LitComposite(Tuple value, TypeAtom composedType) {
		super();
		this.value = value;
		this.composedType = composedType;
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		return new LitComposite((Tuple) this.value.interpret(env), this.composedType);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) {
		return new Pair<Type, Substitution>(this.composedType, Substitution.EMPTY);
	}

	@Override
	public String toClojureCode() throws AppendableException {
		return this.toClojureCode(this.composedType, Environment.topLevelEnvironment);
	}

	@Override
	protected String toClojureCode(Type expectedType, Environment env) throws AppendableException {
		return this.value.toClojureCode(this.value.infer(env).first, env);
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
		StringBuilder s = new StringBuilder("[");
		Iterator<Expression> i = this.value.iterator();
		;
		while (i.hasNext()) {
			s.append(i.next());
			if (i.hasNext()) {
				s.append(' ');
			}
		}
		s.append(']');
		return s.toString();

		// return "<" + this.composedType.toString() + " "
		// + this.value.stream().map(x -> x.toString() + ",").reduce("", (x, y) -> x +
		// y) + ">";
	}
}
