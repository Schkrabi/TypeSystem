package velka.core.application;

import velka.core.expression.Expression;
import velka.core.interpretation.ClojureCoreSymbols;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.util.AppendableException;
import velka.util.Pair;

/**
 * This special form checks if its first argument can be deconstructed as its
 * second argument
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class CanDeconstructAs extends Expression {
	
	/**
	 * Velka symbol for special form can-deconstruct-as
	 */
	public static final String CAN_DECONSTRUCT_AS = "can-deconstruct-as";

	/**
	 * Inspected expression
	 */
	public final Expression expression;
	/**
	 * Tested type
	 */
	public final Type as;

	public CanDeconstructAs(Expression expression, Type as) {
		this.expression = expression;
		this.as = as;
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Expression e = this.expression.interpret(env, typeEnv);
		if (!(e instanceof LitComposite)) {
			return LitBoolean.FALSE;
		}
		LitComposite lc = (LitComposite) e;
		Pair<Type, Substitution> p = lc.value.infer(env, typeEnv);

		if(Type.unifyTypes(p.first, this.as).isEmpty()) {
			return LitBoolean.FALSE;
		}

		return LitBoolean.TRUE;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> p = this.expression.infer(env, typeEnv);
		return new Pair<Type, Substitution>(TypeAtom.TypeBoolNative, p.second);
	}

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		String code = LitBoolean.clojureBooleanToClojureLitBoolean(
				"(.isPresent (velka.lang.types.Type/unifyRepresentation " 
						+ this.as.clojureTypeRepresentation() 
						+ " (" + ClojureCoreSymbols.getTypeClojureSymbol_full + " (first " + this.expression.toClojureCode(env, typeEnv)
						+ "))))");
		
		return code;
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof CanDeconstructAs) {
			return this.expression.equals(((CanDeconstructAs) other).expression)
					&& this.as.equals(((CanDeconstructAs) other).as);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.expression.hashCode() * this.as.hashCode();
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof CanDeconstructAs) {
			int cmp = this.expression.compareTo(((CanDeconstructAs) other).expression);
			if (cmp != 0) {
				return cmp;
			}
			cmp = this.as.compareTo(((CanDeconstructAs) other).as);
			return cmp;
		}
		return super.compareTo(other);
	}
	
	@Override
	public String toString() {
		return "(" + CAN_DECONSTRUCT_AS + " " + this.expression.toString() + " " + this.as.toString() + ")";
	}

}
