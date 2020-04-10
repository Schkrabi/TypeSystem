package application;

import expression.Expression;
import interpretation.Environment;
import semantic.SemanticParserStatic;
import semantic.TypeEnvironment;
import types.Substitution;
import types.Type;
import types.TypeAtom;
import types.TypeName;
import types.TypeRepresentation;
import util.AppendableException;
import util.Pair;

/**
 * Expression for special form defining representations
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class DefineRepresentation extends Expression {

	/**
	 * Name of type for which representation is defined
	 */
	public final TypeName typeName;
	/**
	 * Name of representation which is defined
	 */
	public final TypeRepresentation representation;

	public DefineRepresentation(TypeName typeName, TypeRepresentation representation) {
		super();
		this.typeName = typeName;
		this.representation = representation;
	}

	@Override
	public String toString() {
		return "(" + SemanticParserStatic.DEFINE_REPRESENTATION + " " + this.typeName.toString() + " "
				+ this.representation.toString() + ")";
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof DefineRepresentation) {
			return this.typeName.equals(((DefineRepresentation) other).typeName)
					&& this.representation.equals(((DefineRepresentation) other).representation);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.typeName.hashCode() * this.representation.hashCode();
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof DefineRepresentation) {
			int cmp = this.typeName.compareTo(((DefineRepresentation) other).typeName);
			if (cmp != 0)
				return cmp;
			return this.representation.compareTo(((DefineRepresentation) other).representation);
		}
		return super.compareTo(other);
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		TypeAtom type = new TypeAtom(this.typeName, this.representation);
		TypeEnvironment.singleton.addRepresentation(type);
		return Expression.EMPTY_EXPRESSION;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return Expression.EMPTY_EXPRESSION.infer(env);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		TypeAtom ta = new TypeAtom(this.typeName, this.representation);
		TypeEnvironment.singleton.addRepresentation(ta);
		return "";
	}

}
