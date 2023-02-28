package velka.core.literal;

import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.util.Pair;

/**
 * Class for string literals
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class LitString extends Literal {
	/**
	 * value of the string literal
	 */
	public final String value;

	public LitString(String value) {
		this.value = value;
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) {
		return this;
	}

	@Override
	public String toString() {
		return "[\"" + this.value + "\"]";
	}

	@Override
	public String valueToClojure(Environment env, TypeEnvironment typeEnv) {
		return '"' + this.value + '"';
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
		return new Pair<Type, Substitution>(TypeAtom.TypeStringNative, Substitution.EMPTY);
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof LitString)) {
			return false;
		}
		LitString other = (LitString) o;
		return this.value.equals(other.value);
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof LitString) {
			return this.value.compareTo(((LitString) other).value);
		}
		return super.compareTo(other);
	}

	@Override
	public int hashCode() {
		return this.value.hashCode();
	}
	
	/**
	 * Creates code for string literal in clojure
	 * @param clojureCode code providing string value for literal
	 * @return clojure code
	 */
	public static String clojureLit(String clojureCode) {
		return Literal.clojureValueToClojureLiteral(clojureCode, TypeAtom.TypeStringNative);
	}
}
