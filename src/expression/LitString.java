package expression;

import java.util.Map;
import java.util.TreeMap;

import interpretation.Environment;
import types.Type;
import types.TypeConcrete;

/**
 * Class for string literals
 * @author Mgr. Radomir Skrabal
 *
 */
public class LitString extends Literal {
	/**
	 * value of the string literal
	 */
	public final String value;
	
	public LitString(String value) {
		this.value = value;
	}

	@Override
	public Type getDefaultRepresentationType() {
		return TypeConcrete.TypeString;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		return this;
	}
	
	@Override
	public String toString() {
		return "\"" + this.value + "\"";
	}

	@Override
	public String toClojureCode() throws Exception {
		return '"' + this.value + '"';
	}

	@Override
	public Map<Expression, Type> infer(Environment env) {
		Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
		
		if(this.typeHypothesis == null) {
			this.typeHypothesis = new TreeMap<Expression, Type>();
			this.typeHypothesis.put(this, TypeConcrete.TypeString);
		}
		
		hyp.putAll(this.typeHypothesis);
		return hyp;
	}
	
	@Override
	public boolean equals(Object o){
		if(!(o instanceof LitString)){
			return false;
		}
		LitString other = (LitString) o;
		return this.value.equals(other.value);
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof LitString) {
			return this.value.compareTo(((LitString) other).value);
		}
		return super.compareTo(other);
	}
}
