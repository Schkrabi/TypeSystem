package expression;

import interpretation.Environment;
import types.ForallType;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;

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
	public Type infer() throws Exception{
		Type infered = super.infer();
		if(!infered.isApplicableType()){
			throw new Exception("Badly typed constructor " + this.toString() + " infered to not-Arrow type " + infered);
		}
		if(infered instanceof ForallType) {
			infered = ((ForallType)infered).getBoundType();
		}
		
		Type cType = new TypeArrow(this.argsType, this.constructedType);
		this.setType(cType);
		
		return cType;
	}
}
