package types;

import java.util.Set;
import java.util.TreeSet;

public class TypeConcrete extends Type {
	public final String name;
	
	private TypeConcrete(String name){
		this.name = name;
	}
	
	@Override
	public String toString(){
		return this.name;
	}
	
	@Override
	public boolean equals(Object o){
		if(!(o instanceof TypeConcrete)){
			return false;
		}
		TypeConcrete other = (TypeConcrete)o;
		return this.name.equals(other.name);
	}

	public static final TypeConcrete TypeBool = new TypeConcrete("Bool");
	public static final TypeConcrete TypeInt = new TypeConcrete("Int");

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
		return this.name.compareTo(other.name);
	}	
}
