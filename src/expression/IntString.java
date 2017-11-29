package expression;

import types.Type;
import types.TypeRepresentation;
import interpretation.Environment;

/**
 * Implementation of String Int Literal
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class IntString extends LitInteger {
	/**
	 * Value of the literal
	 */
	public final String value;

	/**
	 * Regular expression for string int value validation
	 */
	private static String intRegExp = "^-?([0-9]{1,9}|[0-1][0-9]{9}|20[0-9]{8}|21[0-3][0-9]{7}|214[0-6][0-9]{6}|2147[0-3][0-9]{5}|21474[0-7][0-9]{4}|214748[0-2][0-9]{3}|2147483[0-5][0-9]{2}|21474836[0-3][0-9]|214748364[0-7])$|^(-2147483648)$";

	public IntString(String value) {
		if (!value.matches(intRegExp)) {
			this.value = "";
			return;
		}
		this.value = value;
	}

	@Override
	public Expression interpret(Environment env) {
		return this;
	}

	@Override
	public String toString() {
		return this.value;
	}

	@Override
	public Type infer() throws Exception {
		this.setType(TypeRepresentation.TypeIntString);
		return TypeRepresentation.TypeIntString;
	}

	@Override
	public Literal fromDefaultRepresentation(Literal l) {
		IntBinary def = (IntBinary) l;
		return new IntString(Integer.toString(def.value));
	}

	@Override
	public Literal toDefaultRepresentation() {
		return new IntBinary(Integer.parseInt(this.value));
	}

	@Override
	public Literal convertRepresentation(Class<? extends Literal> c) throws Exception {
		if (c == IntString.class) {
			return this;
		}
		if (c == IntBinary.class) {
			return this.toDefaultRepresentation();
		}
		if (c == IntRoman.class) {
			return new IntRoman(IntRoman.int2roman(Integer.parseInt(this.value)));
		}
		return super.convertRepresentation(c);
	}
}
