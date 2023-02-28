package velka.core.literal;

import velka.util.Pair;
import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;

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
		return "[" + Long.toString(this.value) + "]";
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
	public static String clojureLit(String clojureCode) {
		return Literal.clojureValueToClojureLiteral(clojureCode, TypeAtom.TypeIntNative);
	}
}
