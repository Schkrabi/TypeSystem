package expression;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeConcrete;
import util.Pair;

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
	public Expression interpret(Environment env) {
		return this;
	}
	
	@Override
	public String toString() {
		return "\"" + this.value + "\"";
	}

	@Override
	public String toClojureCode() {
		return '"' + this.value + '"';
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) {
		return new Pair<Type, Substitution>(TypeConcrete.TypeString, new Substitution());
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
	
	@Override
	public int hashCode() {
		return this.value.hashCode();
	}
}
