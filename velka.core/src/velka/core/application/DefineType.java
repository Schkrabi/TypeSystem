package velka.core.application;

import velka.core.exceptions.DuplicateTypeDefinitionException;
import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeName;
import velka.util.AppendableException;
import velka.util.Pair;

/**
 * Expression for deftype special form
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class DefineType extends Expression {
	
	/**
	 * Symbol for type special form
	 */
	public static final String TYPE = "type";

	/**
	 * Name of type which expression is defining
	 */
	public final TypeName typeName;

	public DefineType(TypeName typeName) {
		super();
		this.typeName = typeName;
	}

	@Override
	public String toString() {
		return "(" + TYPE + " " + this.typeName.toString() + ")";
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof DefineType) {
			return this.typeName.equals(((DefineType) other).typeName);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.typeName.hashCode();
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof DefineType) {
			return this.typeName.compareTo(((DefineType) other).typeName);
		}
		return super.compareTo(other);
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws DuplicateTypeDefinitionException {
		typeEnv.addType(this.typeName);
		return Expression.EMPTY_EXPRESSION;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		return Expression.EMPTY_EXPRESSION.infer(env, typeEnv);
	}

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		typeEnv.addType(this.typeName);
		return "";
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		Expression e = this.interpret(env, typeEnv);
		return e.convert(to, env, typeEnv);
	}

}
