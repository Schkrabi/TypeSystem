package expression;

import types.Substitution;
import types.Type;
import types.TypeConcrete;
import util.Pair;

import expression.Expression;
import expression.Literal;
import interpretation.Environment;

/**
 * Class for floating point number literal
 * @author Mgr. Radomir Skrabal
 *
 */
public class LitDouble extends Literal {
	
	/**
	 * Value of the floating point literal
	 */
	public final double value;
	
	public LitDouble(double value) {
		this.value = value;
	}

	@Override
	public Type getDefaultRepresentationType() {
		return TypeConcrete.TypeDouble;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		return this;
	}

	@Override
	public String toString() {
		return Double.toString(this.value);
	}

	@Override
	public String toClojureCode() throws Exception {
		return Double.toString(this.value);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) {
		return new Pair<Type, Substitution>(TypeConcrete.TypeDouble, new Substitution());
	}
	
	@Override
	public boolean equals(Object o){
		if(!(o instanceof LitDouble)){
			return false;
		}
		LitDouble other = (LitDouble)o;
		return this.value == other.value;
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof LitDouble) {
			return Double.compare(this.value, ((LitDouble) other).value);
		}
		return super.compareTo(other);
	}
}
