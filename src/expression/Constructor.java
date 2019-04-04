package expression;

import interpretation.Environment;
import types.ForallType;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import util.AppendableException;

/**
 * Expression for representation of interpreted constructor
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
	public Type infer(Environment env) throws AppendableException{
		Type infered = super.infer(this.creationEnvironment);
		if(!infered.isApplicableType()){
			throw new AppendableException("Badly typed constructor " + this.toString() + " infered to not-Arrow type " + infered);
		}
		if(infered instanceof ForallType) {
			infered = ((ForallType)infered).getBoundType();
		}
		
		Type cType = new TypeArrow(this.argsType, this.constructedType);
		this.setType(cType);
		
		return cType;
	}
	
	@Override
	public String toString() {
		return "(" + this.constructedType.toString() + " " + this.args.toString() + " " + this.body.toString() + ")";
	}
}
