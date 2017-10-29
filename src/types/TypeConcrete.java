package types;

import java.util.Set;
import java.util.TreeSet;

import expression.IntBinary;
import expression.IntRoman;
import expression.IntString;
import expression.LitBoolean;
import expression.Literal;

public class TypeConcrete extends Type {
	public final String name;
	public final String representation;
	public final Class<? extends Literal> implementation;
	
	private TypeConcrete(String name, Class<? extends Literal> implementation){
		this.name = name;
		this.representation = "";
		this.implementation = implementation;
	}
	
	private TypeConcrete(String name, Class<? extends Literal> implementation, String representation){
		this.name = name;
		this.representation = representation;
		this.implementation = implementation;
	}
	
	@Override
	public String toString(){
		if(this.representation == ""){
			return this.name;
		}
		else {
			return this.name + ":" + this.representation;
		}
	}
	
	@Override
	public boolean equals(Object o){ 
		if(!(o instanceof TypeConcrete)){
			return false;
		}
		TypeConcrete other = (TypeConcrete)o;
		return this.name.equals(other.name) && this.representation.equals(other.representation);
	}
	
	public boolean isSameBasicType(TypeConcrete other){
		return this.name.equals(other.name);
	}

	public static final TypeConcrete TypeBool = new TypeConcrete("Bool", LitBoolean.class);
	public static final TypeConcrete TypeIntBinary = new TypeConcrete("Int", IntBinary.class, "Binary");
	public static final TypeConcrete TypeIntString = new TypeConcrete("Int", IntString.class, "String");
	public static final TypeConcrete TypeIntRoman = new TypeConcrete("Int", IntRoman.class, "Roman");

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
		
		return this.representation.compareTo(other.representation);
	}	
}
