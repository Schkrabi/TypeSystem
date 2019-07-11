package expression;

import java.util.Map;

import interpretation.Environment;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import util.AppendableException;

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
	public Map<Expression, Type> infer(Environment env) throws AppendableException {
		Map<Expression, Type> infered = super.infer(this.creationEnvironment);

		Type cType = new TypeArrow(this.argsType, this.constructedType);

		this.typeHypothesis.put(this, cType);
		infered.put(this, cType);

		return infered;
	}

	@Override
	public String toString() {
		return "(" + this.constructedType.toString() + " " + this.args.toString() + " " + this.body.toString() + ")";
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof Constructor) {
			Constructor o = (Constructor)other;
			int c = this.constructedType.compareTo(o.constructedType);
			if(c != 0)
				return c;
		}
		return super.compareTo(other);
	}
}
