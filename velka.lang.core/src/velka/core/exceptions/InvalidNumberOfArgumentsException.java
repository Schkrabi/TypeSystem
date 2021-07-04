package velka.core.exceptions;

import velka.core.application.Application;
import velka.core.expression.Expression;
import velka.util.AppendableException;

/**
 * Exception for number of arguments mismatch
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class InvalidNumberOfArgumentsException extends AppendableException {
	/**
	 * 
	 */
	private static final long serialVersionUID = 2839790755901401527L;
	/**
	 * Expected number of arguments
	 */
	public final int expected;
	/**
	 * Received number of arguments
	 */
	public final Expression got;
	/**
	 * Application in which mismatch occured
	 */
	public final Application application;

	public InvalidNumberOfArgumentsException(int expected, Expression got, Application application2) {
		super("In " + application2 + "number of arguments mismatch, expected " + expected + " got " + got.toString());
		this.expected = expected;
		this.got = got;
		this.application = application2;
	}
}
