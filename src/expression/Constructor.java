package expression;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import util.AppendableException;
import util.Pair;

/**
 * Expression for representation of interpreted constructor
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Constructor extends Function {

	/**
	 * Constructed type
	 */
	public final Type constructedType;

	public Constructor(TypeTuple argsType, Tuple args, Expression body, Type constructedType,
			Environment createdEnvironment) {
		super(argsType, args, body, createdEnvironment);
		this.constructedType = constructedType;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		try {
			Pair<Type, Substitution> infered = super.infer(env);

			return new Pair<Type, Substitution>(new TypeArrow(((TypeArrow) infered.first).ltype, this.constructedType),
					infered.second);
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public String toString() {
		return "(" + this.constructedType.toString() + " " + this.args.toString() + " " + this.body.toString() + ")";
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof Constructor) {
			Constructor o = (Constructor) other;
			int c = this.constructedType.compareTo(o.constructedType);
			if (c != 0)
				return c;
		}
		return super.compareTo(other);
	}
	
	@Override
	public boolean equals(Object other) {
		if(other instanceof Constructor) {
			return this.constructedType.equals(((Constructor) other).constructedType) && super.equals(other);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return super.hashCode() * this.constructedType.hashCode();
	}
}
