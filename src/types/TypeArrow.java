package types;

public class TypeArrow extends Type {
	public final Type ltype;
	public final Type rtype;
	
	public TypeArrow(Type ltype, Type rtype){
		this.ltype = ltype;
		this.rtype = rtype;
	}
	
	@Override
	public String toString(){
		return this.ltype.toString() + " -> " + this.rtype.toString();
	}
}
