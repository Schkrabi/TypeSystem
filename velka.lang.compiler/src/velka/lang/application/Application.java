package velka.lang.application;


import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;

/**
 * Expression for various kind of application (application of abstractions or
 * special forms)
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Application extends Expression {

	/**
	 * Arguments of the function
	 */
	public final Tuple args;

	public Application(Tuple args) {
		this.args = args;
	}

	/**
	 * Returns name of function or special form of this application
	 * 
	 * @return
	 */
	protected abstract String applicatedToString();

	@Override
	public String toString() {
		return "(" + this.applicatedToString() + " " + this.args.toString() + ")";
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Application) {
			return this.args.equals(((Application) other).args);
		}
		return false;
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof Application) {
			return this.args.compareTo(((Application) other).args);
		}
		return super.compareTo(other);
	}

	@Override
	public int hashCode() {
		return this.args.hashCode();
	}
}
