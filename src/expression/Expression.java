package expression;

import interpretation.Environment;

public abstract class Expression {
	public abstract Expression interpret(Environment env) throws Exception;
}
