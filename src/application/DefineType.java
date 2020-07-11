package application;

import expression.Expression;
import interpretation.Environment;
import semantic.DuplicateTypeDefinitionException;
import semantic.SemanticParserStatic;
import semantic.TypeEnvironment;
import types.Substitution;
import types.Type;
import types.TypeName;
import util.AppendableException;
import util.Pair;

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
	public Expression interpret(Environment env) throws DuplicateTypeDefinitionException {
		TypeEnvironment.singleton.addType(this.typeName);
		return Expression.EMPTY_EXPRESSION;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return Expression.EMPTY_EXPRESSION.infer(env);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		TypeEnvironment.singleton.addType(this.typeName);
		return "";
	}

}
