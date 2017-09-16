package expression;

import interpretation.Environment;

public class Application extends Expression {
	
	public final Expression fun;
	public final Tuple args;
	
	public Application(Expression fun, Expression arg) {
		this.fun = fun;
		this.args = new Tuple(new Expression[]{arg});
	}
	
	public Application(Expression fun, Tuple args){
		this.fun = fun;
		this.args = args;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression ifun = fun.interpret(env);
		
		if(!(ifun instanceof Lambda)) {
			throw new Exception(fun + " is not a fucntion");
		}
		
		Lambda lambda = (Lambda)ifun;
		
		if(lambda.args.values.length != this.args.values.length){
			throw new Exception("In aplication of " + fun + "number of arguments mismatch, expected " + lambda.args.values.length + " got " + this.args.values.length);
		}
		
		Environment childEnv = new Environment(env);
		for(int i = 0; i < lambda.args.values.length; i++){
			childEnv.put((Variable) lambda.args.values[i], this.args.values[i]);
		}
		
		return ((Lambda)ifun).body.interpret(childEnv);
	}

	@Override
	public String toString() {
		return this.fun.toString() + " " + this.args.toString();
	}
}
