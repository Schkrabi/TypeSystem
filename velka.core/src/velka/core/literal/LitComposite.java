package velka.core.literal;

import java.util.List;

import com.sun.codemodel.JExpr;
import com.sun.codemodel.JExpression;

import velka.core.expression.Expression;
import velka.core.interfaces.CompileableToJava;
import velka.core.interpretation.Environment;
import velka.core.langbase.ListNative;
import velka.java.CodeModelInstance;
import velka.java.TypeUtil;
import velka.java.runtime.TypedObject;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.Pair;

/**
 * Literal for representing composed type literals
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class LitComposite extends Literal implements CompileableToJava {

	/**
	 * Composed values of this literal
	 */
	public final Expression value;
	/**
	 * Type of this literal
	 */
	public final TypeAtom composedType;

	public LitComposite(Expression value, TypeAtom composedType) {
		super();
		this.value = value;
		this.composedType = composedType;
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		return new LitComposite(this.value.interpret(env), this.composedType);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) {
		return new Pair<Type, Substitution>(this.composedType, Substitution.EMPTY);
	}

	@Override
	public String valueToClojure(Environment env) throws AppendableException {
		return LitComposite.clojureLit(this.composedType, this.value.toClojureCode(env));
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof LitComposite) {
			return this.composedType.equals(((LitComposite) other).composedType)
					&& this.value.equals(((LitComposite) other).value);
		}
		return false;
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof LitComposite) {
			int cmp = this.composedType.compareTo(((LitComposite) other).composedType);
			if (cmp != 0)
				return cmp;
			return this.value.compareTo(((LitComposite) other).value);
		}
		return super.compareTo(other);
	}

	@Override
	public int hashCode() {
		return this.value.hashCode() * this.composedType.hashCode();
	}

	@Override
	public String toString() {
		return this.value.toString();
	}

	/**
	 * Creates code for composite literal (LitComposite) in clojure.
	 * @param type type of composite literal
	 * @param value value of the composite literal
	 * @return string with code
	 * @throws AppendableException if there is issue with compiling type into clojure
	 */
	public static String clojureLit(Type type, String value) throws AppendableException {
		return ClojureHelper.litCompositeHelper_str(type.clojureTypeRepresentation(), value);
	}

	@Override
	public JExpression toJavaExpr(Environment env) {
		CompileableToJava val = (CompileableToJava)this.value;
		
		var expr = JExpr._new(CodeModelInstance.instance().ref(TypedObject.class))
				.arg(val.toJavaExpr(env))
				.arg(TypeUtil.instance().type2java(this.composedType));
		return expr;
	}
}
