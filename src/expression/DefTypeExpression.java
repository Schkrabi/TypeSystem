package expression;

import interpretation.Environment;
import semantic.TypeEnvironment;
import types.Substitution;
import types.Type;
import types.TypeName;
import util.AppendableException;
import util.Pair;

/**
 * Expression for deftype special form
 * @author Mgr. Radomir Skrabal
 *
 */
public class DefTypeExpression extends Expression {
	
	/**
	 * Name of type which expression is defining
	 */
	public final TypeName typeName;
	
	public DefTypeExpression(TypeName typeName) {
		super();
		this.typeName = typeName;
	}
	
	@Override
	public String toString() {
		return "(deftype " + this.typeName.toString() + ")";
	}
	
	@Override
	public boolean equals(Object other) {
		if(other instanceof DefTypeExpression) {
			return this.typeName.equals(((DefTypeExpression) other).typeName);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return this.typeName.hashCode();
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof DefTypeExpression) {
			return this.typeName.compareTo(((DefTypeExpression) other).typeName);
		}
		return super.compareTo(other);
	}

	@Override
	public Expression interpret(Environment env) {
		TypeEnvironment.singleton.addType(this.typeName);
		return Expression.EMPTY_EXPRESSION;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return Expression.EMPTY_EXPRESSION.infer(env);
	}

	@Override
	public String toClojureCode() throws AppendableException {
		// TODO Auto-generated method stub
		throw new AppendableException("Not Implemented");
	}

}
