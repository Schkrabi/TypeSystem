package velka.lang.parserExceptions;

import velka.lang.util.AppendableException;

public class InvalidNumberOfArgsException extends AppendableException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 9204750752479371874L;

	public final int expected;
	public final int got;

	public InvalidNumberOfArgsException(int expected, int got) {
		super("Got " + got + " arguments, expected " + expected);
		this.expected = expected;
		this.got = got;
	}
}
