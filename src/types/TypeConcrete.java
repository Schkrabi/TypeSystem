package types;

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
}
