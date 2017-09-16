package expression;

import interpretation.Environment;

public class Lambda extends Expression {
	
	public final Tuple args;
	public final Expression body;
	
	public Lambda(Variable arg, Expression body) {
		this.args = new Tuple(new Expression[]{arg});
		this.body = body;
	}
	
	public Lambda(Tuple args, Expression body){
		this.args = args;
		this.body = body;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		return this;
	}

	@Override
	public String toString() {
		return "lambda " + this.args.toString() + " " + this.body.toString();
	}
}
