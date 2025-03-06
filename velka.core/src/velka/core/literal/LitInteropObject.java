package velka.core.literal;

import java.util.List;

import com.sun.codemodel.JExpression;

import velka.core.expression.Expression;
import velka.core.interfaces.CompileableToJava;
import velka.core.interpretation.Environment;
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
public class LitInteropObject extends Literal implements CompileableToJava {

	public static final TypeAtom TypeJavaObject = new TypeAtom(new TypeName("TypeJavaObject"), TypeRepresentation.NATIVE);
	
	public final Type type;
	
	/**
	 * Carried object
	 */
	public final Object javaObject;

	public LitInteropObject(Object javaObject, Type type) {
		this.javaObject = javaObject;
		this.type = type;
	}

	@Override
	protected String valueToClojure(Environment env) throws AppendableException {
		throw new AppendableException(
				"Cannot create clojure code for " + this.getClass().getName() + " of value " + this.toString());
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		return this;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return new Pair<Type, Substitution>(this.type, Substitution.EMPTY);
	}

	@Override
	public String toString() {
//		if(this.type.equals(TypeAtom.TypeListNative)) {
//			@SuppressWarnings("unchecked")
//			var l = (List<Object>)this.javaObject;
//			var ret = l.stream().map(x -> {
//				if(x instanceof String s) {
//					return "\"" + s + "\"";
//				}
//				return x.toString();
//			}).reduce((x, y) -> x + " " + y).get();
//			return ret;
//		}
		
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

	@Override
	public JExpression toJavaExpr(Environment env) {
		throw new RuntimeException("LitInteropObject should not have a static reference.");
	}
}
