package util;

import application.Application;
import expression.Expression;

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

	public InvalidNumberOfArgumentsException(int expected, Expression got, Application abstraction) {
		super("In " + abstraction + "number of arguments mismatch, expected " + expected + " got " + got.toString());
		this.expected = expected;
		this.got = got;
		this.application = abstraction;
	}
}
