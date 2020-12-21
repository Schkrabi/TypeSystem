package velka.lang.literal;

import velka.lang.expression.Expression;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.util.Pair;

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
		return '"' + this.value + '"';
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
}
