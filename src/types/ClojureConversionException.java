package types;

import util.AppendableException;

/**
 * Exception thrown when error occurs during converting types for clojure
 * @author Mgr. Radomir Skrabal
 *
 */
public class ClojureConversionException extends AppendableException {
	/**
	 * 
	 */
	private static final long serialVersionUID = -8834162489130612175L;
	/**
	 * Converted type
	 */
	public final Type fromType;
	/**
	 * Target type
	 */
	public final Type toType;
	/**
	 * Conversion argument
	 */
	public final String argument;
	
	public ClojureConversionException(Type fromType, Type toType, String argument) {
		super("Error when converting type " + fromType.toString() + " to " + toType.toString() + " with argument " + argument + " for clojure.");
		this.fromType = fromType;
		this.toType = toType;
		this.argument = argument;
	}
}
