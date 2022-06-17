package velka.core.literal;

import java.util.List;

import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.langbase.ListNative;
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
public class LitComposite extends Literal {

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
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		return new LitComposite(this.value.interpret(env, typeEnv), this.composedType);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
		return new Pair<Type, Substitution>(this.composedType, Substitution.EMPTY);
	}

	@Override
	public String valueToClojure(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		return this.value.toClojureCode(env, typeEnv);
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

	@SuppressWarnings("unchecked")
	@Override
	public String toString() {
		if(this.composedType == TypeAtom.TypeListNative) {
			LitInteropObject intObj = (LitInteropObject)this.value;
			return "[" + ListNative.toStringListNative((List<Expression>)intObj.javaObject) + "]";
		}
		
		return "[" + this.value.toString() + "]";

		// return "<" + this.composedType.toString() + " "
		// + this.value.stream().map(x -> x.toString() + ",").reduce("", (x, y) -> x +
		// y) + ">";
	}

	/**
	 * Creates code for composite literal (LitComposite) in clojure.
	 * @param type type of composite literal
	 * @param value value of the composite literal
	 * @return string with code
	 * @throws AppendableException if there is issue with compiling type into clojure
	 */
	public static String litCompositeHelper(Type type, String value) throws AppendableException {
		return ClojureHelper.litCompositeHelper_str(type.clojureTypeRepresentation(), value);
	}
}
