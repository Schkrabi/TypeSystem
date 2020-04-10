package abstraction;

import expression.Expression;
import expression.Tuple;
import interpretation.Environment;
import types.Substitution;
import types.Type;
import util.AppendableException;
import util.Pair;

public class ConstructorReference extends Abstraction {

	@Override
	protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
		// TODO Auto-generated method stub
		return null;
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
