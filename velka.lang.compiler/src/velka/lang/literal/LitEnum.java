/**
 * 
 */
package velka.lang.literal;

import velka.lang.expression.Expression;
import velka.lang.interpretation.Environment;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;

/**
 * Class for representing literals of enumerated values
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class LitEnum extends Literal {

	/**
	 * Name/value of this enumerated type
	 */
	public final String value;
	/**
	 * Type of this enumerated value
	 */
	public final TypeAtom enumType;

	public LitEnum(String value, TypeAtom enumType) {
		super();
		this.value = value;
		this.enumType = enumType;
	}

	@Override
	public Expression interpret(Environment env) {
		return this;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) {
		return new Pair<Type, Substitution>(this.enumType, Substitution.EMPTY);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof LitEnum) {
			return this.enumType.equals(((LitEnum) other).enumType) && this.value.equals(((LitEnum) other).value);
		}
		return false;
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof LitEnum) {
			int cmp = this.enumType.compareTo(((LitEnum) other).enumType);
			if (cmp != 0)
				return cmp;
			return this.value.compareTo(((LitEnum) other).value);
		}
		return super.compareTo(other);
	}

	@Override
	public int hashCode() {
		return this.value.hashCode() * this.enumType.hashCode();
	}

	@Override
	public String toString() {
		return this.value;
	}

	@Override
	public String valueToClojure(Environment env) throws AppendableException {
		throw new AppendableException("Not Implemented");
	}

}
