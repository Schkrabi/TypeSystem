package expression;

import types.Type;
import types.TypeArrow;

public class Constructor extends Lambda {
	
	private Type constructedType;

	public Constructor(Tuple args, Expression body) {
		super(args, body);
	}
	
	public Type getConstructedType(){
		return this.constructedType;
	}
	
	public void setConstructedType(Type constructedType){
		this.constructedType = constructedType;
	}

	@Override
	public Type infer() throws Exception{
		Type infered = super.infer();
		if(!(infered instanceof TypeArrow)){
			throw new Exception("Badly typed constructor " + this.toString() + " infered to not-Arrow type " + infered);
		}
		TypeArrow type = (TypeArrow)infered;
		return new TypeArrow(type.ltype, this.constructedType);
	}
	
	@Override
	public String toString(){
		String s = super.toString();
		return "(construct " + this.constructedType.toString() + " " + s + ")";
	}
}
