package expression;

import types.Type;

public abstract class Literal extends Expression {
	
	public abstract Type getDefaultImplementationType();
	public abstract Literal fromDefaultImplementation(Literal l);
	public abstract Literal toDefaultImplementaion();
}
