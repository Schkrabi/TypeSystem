package util;

import expression.Expression;

/**
 * Exception for reporting uncompilable exception being compiled to clojure code
 * @author Mgr. Radomir Skrabal
 *
 */
public class InvalidClojureCompilationException extends AppendableException {

	/**
	 * 
	 */
	private static final long serialVersionUID = -3267231071154413256L;
	/**
	 * Expression causing error
	 */
	public final Expression compiled;
	
	public InvalidClojureCompilationException(Expression compiled) {
		super("You cannot compile " + compiled + "of class " + compiled.getClass().getName() + " to clojure code!");
		this.compiled = compiled;
	}
}
