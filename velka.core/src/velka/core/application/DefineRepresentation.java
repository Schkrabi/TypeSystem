package velka.core.application;

import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.types.TypeName;
import velka.types.TypeRepresentation;
import velka.util.AppendableException;
import velka.util.Pair;

/**
 * Expression for special form defining representations
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class DefineRepresentation extends Expression {
	
	/**
	 * Symbol for representation special form
	 */
	public static final String REPRESENTATION = "representation";

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
		return "(" + REPRESENTATION + " " + this.typeName.toString() + " "
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
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		TypeAtom type = new TypeAtom(this.typeName, this.representation);
		typeEnv.addRepresentation(type);
		return Expression.EMPTY_EXPRESSION;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		return Expression.EMPTY_EXPRESSION.infer(env, typeEnv);
	}

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		TypeAtom ta = new TypeAtom(this.typeName, this.representation);
		typeEnv.addRepresentation(ta);
		return "";
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		Expression e = this.interpret(env, typeEnv);
		return e.convert(to, env, typeEnv);
	}

}
