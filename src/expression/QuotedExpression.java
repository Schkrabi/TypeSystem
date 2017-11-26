package expression;

import interpretation.Environment;
import types.Type;

public class QuotedExpression extends Expression {
	
	public final Expression quoted;
	
	public QuotedExpression(Expression quoted){
		this.quoted = quoted;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		return this.quoted;
	}

	@Override
	public Type infer() throws Exception {
		return this.quoted.infer();
	}
	
	@Override
	public String toString(){
		return "'" + this.quoted.toString();
	}

}
