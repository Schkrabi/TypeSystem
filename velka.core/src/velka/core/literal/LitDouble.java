package velka.core.literal;

import velka.util.Pair;
import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;

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
	public Expression interpret(Environment env, TypeEnvironment typeEnv) {
		return this;
	}

	@Override
	public String toString() {
		return "[" + Double.toString(this.value) + "]";
	}

	@Override
	public String valueToClojure(Environment env, TypeEnvironment typeEnv) {
		return Double.toString(this.value);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
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
	
	/**
	 * Creates code for double literal in clojure
	 * @param clojureCode code providing double value for literal
	 * @return clojure code
	 */
	public static String clojureDoubleToClojureLitDouble(String clojureCode) {
		return Literal.clojureValueToClojureLiteral(clojureCode, TypeAtom.TypeDoubleNative);
	}
}
