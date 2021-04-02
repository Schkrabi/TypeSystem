package velka.lang.literal;

import velka.lang.expression.Expression;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeName;
import velka.lang.types.TypeRepresentation;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;

/**
 * Literal for holding java objects at runtime
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class LitInteropObject extends Literal {

	public static final TypeAtom TypeJavaObject = new TypeAtom(new TypeName("TypeJavaObject"), TypeRepresentation.NATIVE);
	
	/**
	 * Carried object
	 */
	public final Object javaObject;

	public LitInteropObject(Object javaObject) {
		this.javaObject = javaObject;
	}

	@Override
	protected String valueToClojure(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		throw new AppendableException(
				"Cannot create clojure code for " + this.getClass().getName() + " of value " + this.toString());
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		return this;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		return new Pair<Type, Substitution>(TypeJavaObject, Substitution.EMPTY);
	}

	@Override
	public String toString() {
		return this.javaObject.toString();
	}

	@Override
	public int hashCode() {
		return this.javaObject.hashCode();
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof LitInteropObject) {
			return this.javaObject.equals(((LitInteropObject) o).javaObject);
		}
		return false;
	}

	@Override
	public int compareTo(Expression e) {
		if (e instanceof LitInteropObject) {
			// Not a good solution
			int c  = Integer.compare(this.javaObject.hashCode(), ((LitInteropObject) e).javaObject.hashCode());
			return c;
		}

		return super.compareTo(e);
	}
}
