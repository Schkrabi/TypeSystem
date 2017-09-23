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
	
	@Override
	public boolean equals(Object o){
		if(!(o instanceof TypeArrow)){
			return false;
		}
		TypeArrow other = (TypeArrow)o;
		return 		this.ltype.equals(other.ltype) 
				&&	this.rtype.equals(other.rtype);
	}
}
