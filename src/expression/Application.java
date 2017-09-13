package expression;

import interpretation.Environment;

public class Application extends Expression {
	
	public final Expression fun;
	public final Expression arg;
	
	public Application(Expression fun, Expression arg) {
		this.fun = fun;
		this.arg = arg;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression ifun = fun.interpret(env);
		
		if(!(ifun instanceof Lambda)) {
			throw new Exception(fun + " is not a fucntion");
		}
		
		Environment childEnv = new Environment(env);
		childEnv.put(((Lambda)ifun).arg, this.arg);
		return ((Lambda)ifun).body.interpret(childEnv);
	}

	@Override
	public String toString() {
		return this.fun.toString() + " " + this.arg.toString();
	}
}
