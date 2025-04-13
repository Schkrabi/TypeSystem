package velka.core.literal;

import velka.util.ClojureHelper;
import velka.util.Pair;

import com.sun.codemodel.JExpr;
import com.sun.codemodel.JExpression;

import velka.core.expression.Expression;
import velka.core.interfaces.CompileableToJava;
import velka.core.interpretation.Environment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;

/**
 * Abstract class for Integer Literals representations
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class LitInteger extends Literal implements CompileableToJava {

	public final int value;

	public LitInteger(int value) {
		this.value = value;
	}

	@Override
	public Expression interpret(Environment env) {
		return this;
	}

	@Override
	public String valueToClojure(Environment env) {
		return ClojureHelper.applyClojureFunction("int", Integer.toString(this.value));
	}

	@Override
	public String toString() {
		return Integer.toString(this.value);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) {
		return new Pair<Type, Substitution>(TypeAtom.TypeIntNative, Substitution.EMPTY);
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof LitInteger)) {
			return false;
		}
		LitInteger other = (LitInteger) o;
		return this.value == other.value;
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof LitInteger) {
			return Integer.compare(this.value, ((LitInteger) other).value);
		}
		return super.compareTo(other);
	}

	@Override
	public int hashCode() {
		return ((Integer) this.value).hashCode();
	}
	
	/**
	 * Creates code for int literal in clojure
	 * @param clojureCode code providing int value for literal
	 * @return clojure code
	 */
	public static String clojureLit(String clojureCode) {
		return Literal.clojureValueToClojureLiteral(clojureCode, TypeAtom.TypeIntNative);
	}

	@Override
	public JExpression toJavaExpr(Environment env) {
		return JExpr.lit(this.value);
	}
}
