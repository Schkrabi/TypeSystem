package types;

public class TypeVariable extends Type {
	public final String name;
	
	public TypeVariable(String name){
		this.name = name;
	}
	
	@Override
	public String toString(){
		return this.name;
	}
	
	@Override
	public boolean equals(Object o){
		if(!(o instanceof TypeVariable)){
			return false;
		}
		TypeVariable other = (TypeVariable)o;
		return this.name.equals(other.name);
	}
}
