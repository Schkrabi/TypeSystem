package expression;

import types.Substitution;
import types.Type;
import types.TypeAtom;
import util.Pair;

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
	public String toClojureCode() {
		return Boolean.toString(this.value);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) {
		return new Pair<Type, Substitution>(TypeAtom.TypeBoolNative, Substitution.EMPTY);
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
