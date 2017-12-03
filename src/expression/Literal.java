package expression;

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

	/**
	 * Converts literal in default representation to literal with same
	 * representation as this
	 * 
	 * @param l
	 *            literal in default representation
	 * @return new Literal
	 */
	public abstract Literal fromDefaultRepresentation(Literal l);

	/**
	 * Converts this literal do default representation
	 * 
	 * @return new Literal in default representation
	 * @throws Exception
	 */
	public abstract Literal toDefaultRepresentation() throws Exception;

	/**
	 * Converts this literal to different representation
	 * 
	 * @param c
	 *            Class of literal representation
	 * @return new literal object
	 * @throws Exception
	 */
	public Literal convertRepresentation(Class<? extends Literal> c) throws Exception {
		throw new Exception("Unknown literal conversion " + this.getClass().getName() + " to " + c.getName());
	}

	/**
	 * Creates expression that wraps the conversion to default representation for
	 * the interpretation time
	 * 
	 * @param expr
	 *            Expression to convert
	 * @return Expression
	 */
	public static Expression defaultRepresentationLazy(Expression expr) {
		return new ConvertToDefaultRepresentationWrapper(expr);
	}

	/**
	 * Creates expressionthat wraps the conversion for the interpretation time
	 * 
	 * @param expr
	 *            Expression to convert
	 * @param c
	 *            Representation class to convert to
	 * @return Expression
	 */
	public static Expression convertRepresentationLazy(Expression expr, Class<? extends Literal> c) {
		return new ConversionWrapper(expr, c);
	}
	
	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		return this;
	}

	/**
	 * Wrapper for conversion of Literals to their default representation
	 * 
	 * @author Mgr. Radomir Skrabal
	 *
	 */
	private static class ConvertToDefaultRepresentationWrapper extends Expression {

		/**
		 * Wrapped expression
		 */
		private Expression wraped;

		public ConvertToDefaultRepresentationWrapper(Expression wraped) {
			this.wraped = wraped;
		}

		@Override
		public Expression interpret(Environment env) throws Exception {
			Expression e = this.wraped.interpret(env);
			if (!(e instanceof Literal)) {
				return e;
			}

			return ((Literal) e).toDefaultRepresentation();
		}

		@Override
		public Type infer() throws Exception {
			return this.wraped.infer(); // ??
		}

		@Override
		public Expression substituteTopLevelVariables(Environment topLevel) {
			return this;
		}
	}

	/**
	 * Wrapper for conversion of Literals
	 * 
	 * @author Mgr. Radomir Skrabal
	 *
	 */
	private static class ConversionWrapper extends Expression {
		/**
		 * Wrapped expression
		 */
		private Expression wrapped;
		/**
		 * Representation class to convert to
		 */
		private Class<? extends Literal> convertTo;

		public ConversionWrapper(Expression wrapped, Class<? extends Literal> convertTo) {
			this.wrapped = wrapped;
			this.convertTo = convertTo;
		}

		@Override
		public Expression interpret(Environment env) throws Exception {
			Expression e = this.wrapped.interpret(env);
			if (!(e instanceof Literal)) {
				return e;
			}
			return ((Literal) e).convertRepresentation(this.convertTo);
		}

		@Override
		public Type infer() throws Exception {
			return this.wrapped.infer();
		}

		@Override
		public Expression substituteTopLevelVariables(Environment topLevel) {
			return this;
		}
	}
}
