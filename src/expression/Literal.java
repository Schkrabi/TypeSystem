package expression;

import interpretation.Environment;
import types.Type;

public abstract class Literal extends Expression {
	
	public abstract Type getDefaultImplementationType();
	public abstract Literal fromDefaultImplementation(Literal l);
	public abstract Literal toDefaultImplementaion();
	
	public static Expression defaultImplementationLazy(Expression expr){
		return new ConversionWrapper(expr);
	}
	
	private static class ConversionWrapper extends Expression{
		
		private Expression wraped;
		
		public ConversionWrapper(Expression wraped){
			this.wraped = wraped;
		}

		@Override
		public Expression interpret(Environment env) throws Exception {
			Expression e = this.wraped.interpret(env);
			if(!(e instanceof Literal)){
				return e;
			}
			
			return ((Literal)e).toDefaultImplementaion();
		}

		@Override
		public Type infer() throws Exception {
			return this.wraped.infer(); //??
		}
		
	}
}
