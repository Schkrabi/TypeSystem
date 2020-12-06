package velka.lang.literal;

import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.util.Pair;

import velka.lang.expression.Expression;
import velka.lang.interpretation.Environment;
import velka.lang.literal.Literal;

/**
 * Class for floating point number literal
 * 
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
	public Expression interpret(Environment env) {
		return this;
	}

	@Override
	public String toString() {
		return Double.toString(this.value);
	}

	@Override
	public String valueToClojure(Environment env) {
		return Double.toString(this.value);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) {
		return new Pair<Type, Substitution>(TypeAtom.TypeDoubleNative, Substitution.EMPTY);
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof LitDouble)) {
			return false;
		}
		LitDouble other = (LitDouble) o;
		return this.value == other.value;
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof LitDouble) {
			return Double.compare(this.value, ((LitDouble) other).value);
		}
		return super.compareTo(other);
	}

	@Override
	public int hashCode() {
		return ((Double) this.value).hashCode();
	}
}
