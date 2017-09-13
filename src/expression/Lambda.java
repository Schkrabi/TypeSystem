package expression;

import interpretation.Environment;

public class Lambda extends Expression {
	
	public final Variable arg;
	public final Expression body;
	
	public Lambda(Variable arg, Expression body) {
		this.arg = arg;
		this.body = body;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		return this;
	}

	@Override
	public String toString() {
		return "lambda " + this.arg.toString() + " " + this.body.toString();
	}
}
