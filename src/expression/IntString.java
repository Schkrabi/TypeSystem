package expression;

import types.Type;
import types.TypeConcrete;
import interpretation.Environment;

public class IntString extends LitInteger {
	public final String value;
	
	public IntString(String value){
		if(!value.matches("^-?([0-9]{1,9}|[0-1][0-9]{9}|20[0-9]{8}|21[0-3][0-9]{7}|214[0-6][0-9]{6}|2147[0-3][0-9]{5}|21474[0-7][0-9]{4}|214748[0-2][0-9]{3}|2147483[0-5][0-9]{2}|21474836[0-3][0-9]|214748364[0-7])$|^(-2147483648)$")){
			this.value = "";
			return;
		}
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
	
	@Override
	public Type infer() throws Exception {
		this.setType(TypeConcrete.TypeIntString);
		return TypeConcrete.TypeIntString;
	}
}
