package velka.core.application;

import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitBoolean;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.Pair;

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
				"(.isPresent (velka.types.Type/unifyRepresentation "
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

	@Override
	protected Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		Expression e = this.interpret(env, typeEnv);
		return e.convert(to, env, typeEnv);
	}

}
