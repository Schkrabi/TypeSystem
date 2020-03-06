/**
 * 
 */
package expression;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeAtom;
import util.AppendableException;
import util.Pair;

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
	public String toClojureCode() throws AppendableException {
		// TODO Auto-generated method stub
		return this.value;
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
	protected String toClojureCode(Type expectedType, Environment env) throws AppendableException {
		throw new AppendableException("Not Implemented");
	}

}
