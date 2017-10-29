package expression;

import interpretation.Environment;
import types.Type;

public abstract class Literal extends Expression {
	
	public abstract Type getDefaultRepresentationType();
	public abstract Literal fromDefaultRepresentation(Literal l);
	public abstract Literal toDefaultRepresentation();
	
	public Literal convertRepresentation(Class<? extends Literal> c) throws Exception{
		throw new Exception(this.getClass().getName() + " conversion to unknown representation " + c.getName());
	}
	
	public static Expression defaultRepresentationLazy(Expression expr){
		return new ConvertToDefaultRepresentationWrapper(expr);
	}
	
	public static Expression convertRepresentationLazy(Expression expr, Class<? extends Literal> c){
		return new ConversionWrapper(expr, c);
	}
	
	private static class ConvertToDefaultRepresentationWrapper extends Expression{
		
		private Expression wraped;
		
		public ConvertToDefaultRepresentationWrapper(Expression wraped){
			this.wraped = wraped;
		}

		@Override
		public Expression interpret(Environment env) throws Exception {
			Expression e = this.wraped.interpret(env);
			if(!(e instanceof Literal)){
				return e;
			}
			
			return ((Literal)e).toDefaultRepresentation();
		}

		@Override
		public Type infer() throws Exception {
			return this.wraped.infer(); //??
		}
		
	}
	
	private static class ConversionWrapper extends Expression{
		private Expression wrapped;
		private Class<? extends Literal> convertTo;
		
		public ConversionWrapper(Expression wrapped, Class<? extends Literal> convertTo){
			this.wrapped = wrapped;
			this.convertTo = convertTo;
		}
		
		@Override
		public Expression interpret(Environment env) throws Exception {
			Expression e = this.wrapped.interpret(env);
			if(!(e instanceof Literal)){
				return e;
			}
			return ((Literal)e).convertRepresentation(this.convertTo);
		}

		@Override
		public Type infer() throws Exception {
			return this.wrapped.infer();
		}
		
	}
}
