package application;

import expression.Expression;
import interpretation.Environment;
import types.Substitution;
import types.Type;
import util.AppendableException;
import util.Pair;

public class CanDeconstructAs extends Expression {
	
	/**
	 * Inspected expression
	 */
	public final Expression expression;
	/**
	 * Tested type
	 */
	public final Type as;
	
	public CanDeconstructAs(Expression expression, Type as) {
		this.expression = expression;
		this.as = as;
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		// TODO Auto-generated method stub
		return null;
	}

}
