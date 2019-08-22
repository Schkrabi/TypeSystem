package expression;

import types.Substitution;
import types.Type;
import types.TypeConcrete;
import util.Pair;

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

	@Override
	public String toClojureCode() {
		return Integer.toString(this.value);
	}
	
	@Override
	public String toString(){
		return Integer.toString(this.value);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) {
		return new Pair<Type, Substitution>(TypeConcrete.TypeInt, new Substitution());
	} 
	
	@Override
	public boolean equals(Object o){
		if(!(o instanceof LitInteger)){
			return false;
		}
		LitInteger other = (LitInteger) o;
		return this.value == other.value;
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof LitInteger) {
			return Integer.compare(this.value, ((LitInteger) other).value);
		}
		return super.compareTo(other);
	}
	
	@Override
	public int hashCode() {
		return ((Integer)this.value).hashCode();
	}
}
