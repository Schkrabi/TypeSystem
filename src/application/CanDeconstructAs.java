package application;

import expression.Expression;
import interpretation.Environment;
import literal.LitBoolean;
import literal.LitComposite;
import types.Substitution;
import types.Type;
import types.TypeAtom;
import util.AppendableException;
import util.Pair;

/**
 * This special form checks if its first argument can be deconstructed as its
 * second argument
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class CanDeconstructAs extends Expression {

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
	public Expression interpret(Environment env) throws AppendableException {
		Expression e = this.expression.interpret(env);
		if (!(e instanceof LitComposite)) {
			return LitBoolean.FALSE;
		}
		LitComposite lc = (LitComposite) e;
		Pair<Type, Substitution> p = lc.value.infer(env);

		try {
			Type.unify(p.first, this.as);
		} catch (AppendableException ae) {
			return LitBoolean.FALSE;
		}

		return LitBoolean.TRUE;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		Pair<Type, Substitution> p = this.expression.infer(env);
		return new Pair<Type, Substitution>(TypeAtom.TypeBoolNative, p.second);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		return "(= " + this.as.clojureTypeRepresentation() + " (:lang-type (meta (get "
				+ this.expression.toClojureCode(env) + "))))";
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

}
