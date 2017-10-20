package expression;

import types.Type;
import interpretation.Environment;

public abstract class Expression {
	private Type inferedType = null;
	
	public abstract Expression interpret(Environment env) throws Exception;
	public abstract Type infer() throws Exception;
	
	public Type getType(){
		return this.inferedType;
	}
	
	protected void setType(Type value){
		this.inferedType = value;
	}
}
