package expression;

import interpretation.Environment;

public class IntRoman extends LitInteger {
	public final String value;
	
	public IntRoman(String value){
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
	
	public static int roman2int(IntRoman value){
		//TODO implement
		return 0;
	}
}
