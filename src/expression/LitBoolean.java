package expression;

import types.Type;
import types.TypeConcrete;
import java.util.Map;
import java.util.TreeMap;

import interpretation.Environment;

/**
 * Boolean literal implementation
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class LitBoolean extends Literal {

	/**
	 * Literal value
	 */
	public final boolean value;

	private LitBoolean(boolean value) { 
		this.value = value;
	}

	@Override
	public Expression interpret(Environment env) {
		return this;
	}

	@Override
	public String toString() {
		return Boolean.toString(this.value);
	}

	public static final LitBoolean TRUE = new LitBoolean(true);
	public static final LitBoolean FALSE = new LitBoolean(false);

	@Override
	public Type getDefaultRepresentationType() {
		return TypeConcrete.TypeBool;
	}

	@Override
	public String toClojureCode() throws Exception {
		return Boolean.toString(this.value);
	}

	@Override
	public Map<Expression, Type> infer(Environment env) {
		Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
		
		if(this.typeHypothesis == null) {
			this.typeHypothesis = new TreeMap<Expression, Type>();
			this.typeHypothesis.put(this, TypeConcrete.TypeBool);
		}
		
		hyp.putAll(this.typeHypothesis);
		return hyp;
	}
	
	@Override
	public boolean equals(Object o){
		if(!(o instanceof LitBoolean)){
			return false;
		}
		LitBoolean other = (LitBoolean) o;
		return this.value == other.value;
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof LitBoolean) {
			LitBoolean o = (LitBoolean)other;
			if(this.value == o.value)
				return 0;
			if(this.value)
				return 1;
			return -1;
		}
		return super.compareTo(other);
	}
}
