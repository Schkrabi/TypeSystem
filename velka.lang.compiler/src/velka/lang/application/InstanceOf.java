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
 * instance-of special form
 * @author Mgr. Radomir Skrabal
 *
 */
public class InstanceOf extends Expression {
	
	/**
	 * Type against expression is tested
	 */
	public final Type type;
	/**
	 * Tested expression
	 */
	public final Expression expression;
	
	public InstanceOf(Expression expression, Type type) {
		this.type = type;
		this.expression = expression;
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> infered = this.expression.infer(env, typeEnv);
		
		try {
			Type.unifyTypes(infered.first, this.type);
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
						+ "(if (= (velka.lang.types.Type/unifyTypes " 
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
		if(other instanceof InstanceOf) {
			return this.expression.equals(((InstanceOf) other).expression) & this.type.equals(((InstanceOf) other).type);
		}
		return false;
	}
	
	@Override
	public String toString() {
		return "(instance-of " + this.expression.toString() + " " + this.type.toString() + ")";
	}
	
	@Override
	public int compareTo(Expression o) {
		if(o instanceof InstanceOf) {
			int cmp = this.expression.compareTo(((InstanceOf) o).expression);
			if(cmp != 0) {
				return cmp;
			}
			cmp = this.type.compareTo(((InstanceOf) o).type);
			return cmp;
		}
		return super.compareTo(o);
	}

}
