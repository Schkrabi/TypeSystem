package velka.core.application;

import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.literal.LitBoolean;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.Pair;

/**
 * instance-of special form
 * @author Mgr. Radomir Skrabal
 *
 */
public class InstanceOf extends Expression {
	
	/**
	 * Symbol for special form instance-of
	 */
	public static final String INSTANCE_OF = "instance-of";
	
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
	public Expression interpret(Environment env) throws AppendableException {
		Pair<Type, Substitution> infered = this.expression.interpret(env).infer(env);
		
		if(Type.unifyTypes(infered.first, this.type).isPresent()) {
			return LitBoolean.TRUE;
		}
		return LitBoolean.FALSE;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		Pair<Type, Substitution> p = this.expression.infer(env);
		return new Pair<Type, Substitution>(TypeAtom.TypeBoolNative, p.second);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		String code = LitBoolean.clojureLit(
				"(.isPresent (velka.types.Type/unifyTypes "
				+ "(" + ClojureCoreSymbols.getTypeClojureSymbol_full + " " + this.expression.toClojureCode(env) + ") "
				+ this.type.clojureTypeRepresentation() + "))"); 
				
				return code;
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

	@Override
	protected Expression doConvert(Type from, Type to, Environment env)
			throws AppendableException {
		Expression e = this.interpret(env);
		return e.convert(to, env);
	}

}
