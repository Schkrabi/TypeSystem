package velka.lang.util;

/**
 * Exception that allows appending to its message
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class AppendableException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = -7803153197339136659L;

	private String messageSuffix = "";

	public AppendableException() {
		super();
	}

	public AppendableException(String message) {
		super(message);
	}

	@Override
	public String getMessage() {
		return super.getMessage() + this.messageSuffix;
	}

	/**
	 * Appends additional suffix to the messahe
	 * 
	 * @param s appended string
	 */
	public void appendMessage(String s) {
		this.messageSuffix = this.messageSuffix + s;
	}
}
