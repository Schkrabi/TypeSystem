package expression;

/**
 * Abstract expression class for literals implementations
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Literal extends Expression {
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
	}
}
