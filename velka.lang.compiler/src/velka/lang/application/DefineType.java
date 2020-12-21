package velka.lang.application;

import velka.lang.expression.Expression;
import velka.lang.interpretation.Environment;
import velka.lang.exceptions.DuplicateTypeDefinitionException;
import velka.lang.semantic.SemanticParserStatic;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeName;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;

/**
 * Expression for deftype special form
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class DefineType extends Expression {

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
		return "(" + SemanticParserStatic.DEFINE_TYPE + " " + this.typeName.toString() + ")";
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

}
