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
}
