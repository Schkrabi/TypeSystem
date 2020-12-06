package velka.lang.literal;

import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.util.Pair;
import velka.lang.expression.Expression;
import velka.lang.interpretation.Environment;

/**
 * Abstract class for Integer Literals representations
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class LitInteger extends Literal {

	public final int value;

	public LitInteger(int value) {
		this.value = value;
	}

	@Override
	public Expression interpret(Environment env) {
		return this;
	}

	@Override
	public String valueToClojure(Environment env) {
		return Integer.toString(this.value);
	}

	@Override
	public String toString() {
		return Integer.toString(this.value);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) {
		return new Pair<Type, Substitution>(TypeAtom.TypeIntNative, Substitution.EMPTY);
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof LitInteger)) {
			return false;
		}
		LitInteger other = (LitInteger) o;
		return this.value == other.value;
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof LitInteger) {
			return Integer.compare(this.value, ((LitInteger) other).value);
		}
		return super.compareTo(other);
	}

	@Override
	public int hashCode() {
		return ((Integer) this.value).hashCode();
	}
}
