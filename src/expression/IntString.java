package expression;

import interpretation.Environment;

public class IntString extends LitInteger {
	public final String value;
	
	public IntString(String value){
		//TODO check regex
		super(0); //TODO hotfix, change implementation of LitInteger
		this.value = value;
	}
	
	@Override
	public Expression interpret(Environment env) {
		return this;
	}
	
	@Override
	public String toString(){
		return this.value;
	}
	
	//TODO Infer
}
