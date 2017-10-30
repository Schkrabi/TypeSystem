package expression;

import types.ForallType;
import types.Type;
import types.TypeArrow;
import types.TypeVariable;
import interpretation.Environment;

/**
 * Simple lambda expression
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Lambda extends ExtendedLambda {

	public Lambda(Variable arg, Expression body) {
		super(new Tuple(new Expression[] { arg }), body);
	}

	public Lambda(Tuple args, Expression body) {
		super(args, body);
	}

	/**
	 * Returns the body of the lambda expression
	 * 
	 * @return Expression
	 */
	public Expression getBody() {
		return this.defaultImplementation;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		return this;
	}

	@Override
	public String toString() {
		return "lambda " + this.args.toString() + " " + this.getBody().toString();
	}

	@Override
	public Type infer() throws Exception {
		Type argsType = this.args.infer();
		Type bodyType = this.getBody().infer();

		Type t = new TypeArrow(argsType, bodyType);

		for (TypeVariable v : t.getUnconstrainedVariables()) {
			t = new ForallType(v, t);
		}

		this.setType(t);

		return t;
	}
}
