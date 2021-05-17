package velka.lang.literal;

import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.util.Pair;
import velka.lang.expression.Expression;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;

/**
 * Abstract class for Integer Literals representations
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class LitInteger extends Literal {

	public final long value;

	public LitInteger(long value) {
		this.value = value;
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) {
		return this;
	}

	@Override
	public String valueToClojure(Environment env, TypeEnvironment typeEnv) {
		return Long.toString(this.value);
	}

	@Override
	public String toString() {
		return Long.toString(this.value);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
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
			return Long.compare(this.value, ((LitInteger) other).value);
		}
		return super.compareTo(other);
	}

	@Override
	public int hashCode() {
		return ((Long) this.value).hashCode();
	}
	
	/**
	 * Creates code for int literal in clojure
	 * @param clojureCode code providing int value for literal
	 * @return clojure code
	 */
	public static String clojureIntToClojureLitInteger(String clojureCode) {
		return Literal.clojureValueToClojureLiteral(clojureCode, TypeAtom.TypeIntNative);
	}
}
