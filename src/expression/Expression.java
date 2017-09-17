package expression;

import types.Type;
import interpretation.Environment;

public abstract class Expression {
	public abstract Expression interpret(Environment env) throws Exception;
	public abstract Type infer() throws Exception;
}
