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
	
	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		return this;
	}
	
	public void setLiteralType(Type type){
		this.setType(type);
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
	}
}

