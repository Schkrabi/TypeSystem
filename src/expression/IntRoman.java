package expression;

import types.Type;
import types.TypeConcrete;
import interpretation.Environment;

/**
 * Implementation of Roman Integer Literal
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class IntRoman extends LitInteger {
	/**
	 * The value of literal
	 */
	public final String value;

	/**
	 * Regular expression for roman number validity checking
	 */
	private static final String romanRegExp = "(^(?=[MDCLXVI])M*(C[MD]|D?C{0,3})(X[CL]|L?X{0,3})(I[XV]|V?I{0,3})$)";

	public IntRoman(String value) {
		if (!value.matches(romanRegExp)) {
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

	/**
	 * Converts roman number in string to integer
	 * 
	 * @param str
	 *            String containing the roman integer
	 * @return int value of the integer
	 * @throws Exception
	 *             thrown is str is not a roman number
	 */
	public static int roman2int(String str) throws Exception {
		if (!str.matches(romanRegExp)) {
			throw new Exception("Invalid roman number " + str);
		}

		int res = 0;

		for (int i = 0; i < str.length(); i++) {
			int s1 = value(str.charAt(i));

			if (i + 1 < str.length()) {
				int s2 = value(str.charAt(i + 1));

				if (s1 >= s2) {
					res = res + s1;
				} else {
					res = res + s2 - s1;
					i++;
				}
			} else {
				res = res + s1;
				i++;
			}
		}

		return res;
	}

	/**
	 * Used for int2roman conversion
	 */
	private static final String[] hundreds = { "", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM" };
	/**
	 * Used for int2roman conversion
	 */
	private static final String[] tens = { "", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC" };
	/**
	 * Used for int2roman conversion
	 */
	private static final String[] ones = { "", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX" };

	/**
	 * Converts integer to roman number string
	 * 
	 * @param val
	 *            value to be converted
	 * @return String object containing roman number
	 */
	public static String int2roman(int val) {
		int tmpVal = val;
		StringBuilder s = new StringBuilder();

		while (tmpVal > 1000) {
			s.append("M");
			tmpVal -= 1000;
		}

		s.append(hundreds[tmpVal / 100]);
		tmpVal = tmpVal % 100;
		s.append(tens[tmpVal / 10]);
		tmpVal = tmpVal % 10;
		s.append(ones[tmpVal]);

		return s.toString();
	}

	/**
	 * This function returns value of a Roman symbol
	 * 
	 * @param r
	 *            character to convert
	 * @return int value
	 */
	private static int value(char r) {
		if (r == 'I')
			return 1;
		if (r == 'V')
			return 5;
		if (r == 'X')
			return 10;
		if (r == 'L')
			return 50;
		if (r == 'C')
			return 100;
		if (r == 'D')
			return 500;
		if (r == 'M')
			return 1000;
		return -1;
	}

	@Override
	public Type infer() throws Exception {
		this.setType(TypeConcrete.TypeIntRoman);
		return TypeConcrete.TypeIntRoman;
	}

	@Override
	public Literal fromDefaultRepresentation(Literal l) {
		IntBinary def = (IntBinary) l;
		return new IntRoman(IntRoman.int2roman(def.value));
	}

	@Override
	public Literal toDefaultRepresentation() throws Exception {
		return new IntBinary(IntRoman.roman2int(this.value));
	}

	@Override
	public Literal convertRepresentation(Class<? extends Literal> c) throws Exception {
		if (c == IntRoman.class) {
			return this;
		}
		if (c == IntBinary.class) {
			return this.toDefaultRepresentation();
		}
		if (c == IntString.class) {
			return new IntString(Integer.toString(IntRoman.roman2int(this.value)));
		}
		return super.convertRepresentation(c);
	}
}
