package velka.lang.application;

import velka.lang.expression.Expression;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.literal.LitBoolean;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypesDoesNotUnifyException;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;

/**
 * instance-of-representation special form
 * @author Mgr. Radomir Skrabal
 *
 */
public class InstanceOfRepresentation extends Expression {
	
	/**
	 * Type against expression is tested
	 */
	public final Type type;
	/**
	 * Tested expression
	 */
	public final Expression expression;
	
	public InstanceOfRepresentation(Expression expression, Type type) {
		this.type = type;
		this.expression = expression;
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> infered = this.expression.interpret(env, typeEnv).infer(env, typeEnv);
		
		try {
			Type.unifyRepresentation(infered.first, this.type);
			return LitBoolean.TRUE;
		}catch(TypesDoesNotUnifyException e) {
			return LitBoolean.FALSE;
		}
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> p = this.expression.infer(env, typeEnv);
		return new Pair<Type, Substitution>(TypeAtom.TypeBoolNative, p.second);
	}

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		String code = "(try " 
				+ "(if (= (velka.lang.types.Type/unifyRepresentation " 
						+ "(:lang-type (meta " + this.expression.toClojureCode(env, typeEnv) + ")) " 
						+ this.type.clojureTypeRepresentation()
						+ ") velka.lang.types.Substitution/EMPTY) "
					+ LitBoolean.TRUE.toClojureCode(env, typeEnv) + " "
					+ LitBoolean.TRUE.toClojureCode(env, typeEnv) + ")"
				+ "(catch velka.lang.types.TypesDoesNotUnifyException e " + LitBoolean.FALSE.toClojureCode(env, typeEnv) + "))";
		return "(with-meta " + code + " {:lang-type " + TypeAtom.TypeBoolNative.clojureTypeRepresentation() + "})";
	}
	
	@Override
	public int hashCode() {
		return this.expression.hashCode() * this.type.hashCode();
	}
	
	@Override
	public boolean equals(Object other) {
		if(other instanceof InstanceOfRepresentation) {
			return this.expression.equals(((InstanceOfRepresentation) other).expression) & this.type.equals(((InstanceOfRepresentation) other).type);
		}
		return false;
	}
	
	@Override
	public String toString() {
		return "(instance-of-representation " + this.expression.toString() + " " + this.type.toString() + ")";
	}
	
	@Override
	public int compareTo(Expression o) {
		if(o instanceof InstanceOfRepresentation) {
			int cmp = this.expression.compareTo(((InstanceOfRepresentation) o).expression);
			if(cmp != 0) {
				return cmp;
			}
			cmp = this.type.compareTo(((InstanceOfRepresentation) o).type);
			return cmp;
		}
		return super.compareTo(o);
	}

}
