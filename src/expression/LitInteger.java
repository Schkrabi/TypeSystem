package expression;

import types.Type;
import types.TypeConcrete;
import java.util.Map;
import java.util.TreeMap;

import interpretation.Environment;

/**
 * Abstract class for Integer Literals representations
 * @author Mgr. Radomir Skrabal
 *
 */
public class LitInteger extends Literal {
	
	public final int value;
	
	public LitInteger(int value){
		this.value = value;
	}

	@Override
	public Expression interpret(Environment env) {
		return this;
	}
	
	public static LitInteger initializeDefaultImplementation(int value){
		LitInteger l = new LitInteger(value);;
		return l;
	}
	
	@Override
	public Type getDefaultRepresentationType() {
		return TypeConcrete.TypeInt;
	}

	@Override
	public String toClojureCode() throws Exception {
		return Integer.toString(this.value);
	}
	
	@Override
	public String toString(){
		return Integer.toString(this.value);
	}

	@Override
	public Map<Expression, Type> infer(Environment env) {
		Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
		
		if(this.typeHypothesis == null) {
			this.typeHypothesis = new TreeMap<Expression, Type>();
			this.typeHypothesis.put(this, TypeConcrete.TypeInt);
		}
		
		hyp.putAll(this.typeHypothesis);
		return hyp;
	} 
	
	@Override
	public boolean equals(Object o){
		if(!(o instanceof LitInteger)){
			return false;
		}
		LitInteger other = (LitInteger) o;
		return this.value == other.value;
	}
}
