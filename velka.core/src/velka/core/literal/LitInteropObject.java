package velka.core.literal;

import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.types.TypeName;
import velka.types.TypeRepresentation;
import velka.util.AppendableException;
import velka.util.Pair;

/**
 * Literal for holding java objects at runtime
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class LitInteropObject extends Literal {

	public static final TypeAtom TypeJavaObject = new TypeAtom(new TypeName("TypeJavaObject"), TypeRepresentation.NATIVE);
	
	public final TypeAtom type;
	
	/**
	 * Carried object
	 */
	public final Object javaObject;

	public LitInteropObject(Object javaObject, TypeAtom type) {
		this.javaObject = javaObject;
		this.type = type;
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
		return new Pair<Type, Substitution>(this.type, Substitution.EMPTY);
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
