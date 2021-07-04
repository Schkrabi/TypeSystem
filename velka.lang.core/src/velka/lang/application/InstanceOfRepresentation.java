package velka.lang.application;

import velka.lang.expression.Expression;
import velka.lang.interpretation.ClojureCoreSymbols;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.literal.LitBoolean;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;

/**
 * instance-of-representation special form
 * @author Mgr. Radomir Skrabal
 *
 */
public class InstanceOfRepresentation extends Expression {
	
	/**
	 * velka symbol for special form
	 */
	public static final String INSTANCE_OF_REPRESENTATION = "instance-of-representation";
	
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
		
		if(Type.unifyRepresentation(infered.first, this.type).isPresent()) {
			return LitBoolean.TRUE;
		}
		return LitBoolean.FALSE;
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
				+ "(" + ClojureCoreSymbols.getTypeClojureSymbol_full + " " + this.expression.toClojureCode(env, typeEnv) + ") "
				+ this.type.clojureTypeRepresentation() + "))"); 
				
				return code;
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
		return "(" + INSTANCE_OF_REPRESENTATION + " " + this.expression.toString() + " " + this.type.toString() + ")";
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
