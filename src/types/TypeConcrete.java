package types;

import java.util.Set;
import java.util.TreeSet;

public class TypeConcrete extends Type {
	public final String name;
	public final String implementation;
	
	private TypeConcrete(String name){
		this.name = name;
		this.implementation = "";
	}
	
	private TypeConcrete(String name, String implementation){
		this.name = name;
		this.implementation = implementation;
	}
	
	@Override
	public String toString(){
		if(this.implementation == ""){
			return this.name;
		}
		else {
			return this.name + ":" + this.implementation;
		}
	}
	
	@Override
	public boolean equals(Object o){ 
		if(!(o instanceof TypeConcrete)){
			return false;
		}
		TypeConcrete other = (TypeConcrete)o;
		return this.name.equals(other.name) && this.implementation.equals(other.implementation);
	}
	
	public boolean isSameBasicType(TypeConcrete other){
		return this.name.equals(other.name);
	}

	public static final TypeConcrete TypeBool = new TypeConcrete("Bool");
	//public static final TypeConcrete TypeInt = new TypeConcrete("Int");
	public static final TypeConcrete TypeIntBinary = new TypeConcrete("Int", "Binary");
	public static final TypeConcrete TypeIntString = new TypeConcrete("Int", "String");
	public static final TypeConcrete TypeIntRoman = new TypeConcrete("Int", "Roman");

	@Override
	public Set<TypeVariable> getUnconstrainedVariables() {
		Set<TypeVariable> s = new TreeSet<TypeVariable>();
		return s;
	}

	@Override
	public int compareTo(Type o) {
		if(!(o instanceof TypeConcrete)){
			return super.compareTo(o);
		}
		TypeConcrete other = (TypeConcrete)o;
		int cmp = this.name.compareTo(other.name);
		if(cmp != 0) {
			return cmp;
		}
		
		return this.implementation.compareTo(other.implementation);
	}	
}
