package expression;

import java.util.Arrays;
import java.util.List;

import conversions.IntRomanToIntStringWrapper;
import conversions.IntRomanToIntWrapper;
import conversions.IntStringToIntRomanWrapper;
import conversions.IntStringToIntWrapper;
import conversions.IntToIntRomanWrapper;
import conversions.IntToIntStringWrapper;
import interpretation.Environment;
import types.Type;

/**
 * Abstract expression class for literals implementations
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Literal extends Expression {

	/**
	 * Gets the Type of default representation of this literal
	 * 
	 * @return Type object
	 */
	public abstract Type getDefaultRepresentationType();

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		return this;
	}

	private static List<Class<? extends Literal>> literalOrdering = Arrays.asList(LitInteger.class, LitDouble.class,
			LitString.class, LitBoolean.class);

	@Override
	public int compareTo(Expression other) {
		if (other instanceof Literal) {
			return (int) Math
					.signum(literalOrdering.indexOf(this.getClass()) - literalOrdering.indexOf(other.getClass()));
		}
		return super.compareTo(other);
	}

	/**
	 * Wrapper for conversions
	 * 
	 * @author Mgr. Radomir Skrabal
	 *
	 */
	public abstract static class ConversionWrapper extends Expression {
		/**
		 * Unified conversion argument
		 */
		protected static final Expression arg = new Variable("_x");

		private static List<Class<? extends ConversionWrapper>> conversionOrdering = Arrays.asList(
				IntRomanToIntStringWrapper.class, IntRomanToIntWrapper.class, IntStringToIntWrapper.class,
				IntStringToIntRomanWrapper.class, IntToIntRomanWrapper.class, IntToIntStringWrapper.class);

		public int compareTo(Expression other) {
			if (other instanceof ConversionWrapper) {
				return (int) Math.signum(
						conversionOrdering.indexOf(this.getClass()) - conversionOrdering.indexOf(other.getClass()));
			}
			return super.compareTo(other);
		}
	}
}
