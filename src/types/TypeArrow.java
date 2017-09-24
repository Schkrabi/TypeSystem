package types;

import java.util.Set;
import java.util.TreeSet;

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

	@Override
	public Set<TypeVariable> getUnconstrainedVariables() {
		Set<TypeVariable> s = new TreeSet<TypeVariable>();
		if(this.getRep() != this){
			s.addAll(this.getRep().getUnconstrainedVariables());
			return s;
		}
		
		s.addAll(this.ltype.getUnconstrainedVariables());
		s.addAll(this.rtype.getUnconstrainedVariables());
		return s;
	}
	
	@Override
	public boolean isApplicableType(){
		return true;
	}
}
