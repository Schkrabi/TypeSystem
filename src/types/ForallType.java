package types;

import java.util.Set;
import java.util.TreeSet;

public class ForallType extends Type {
	public final TypeVariable bound;
	public final Type type;
	
	public ForallType(TypeVariable bound, Type type){
		this.bound = bound;
		this.type = type;
	}
	
	@Override
	public String toString(){
		return "Forall " + this.bound.toString() + " (" + this.type.toString() + ")";
	}
	
	@Override
	public boolean equals(Object o){
		if(!(o instanceof ForallType)){
			return false;
		}
		ForallType other = (ForallType)o;
		return 		this.bound.equals(other.bound)
				&&	this.type.equals(other.type);
	}

	@Override
	public Set<TypeVariable> getUnconstrainedVariables() {
		Set<TypeVariable> s = new TreeSet<TypeVariable>();
		if(this.getRep() != this){
			s.addAll(this.getRep().getUnconstrainedVariables());
			return s;
		}
		
		s.addAll(this.type.getUnconstrainedVariables());
		s.remove(this.bound);
		return s;
	}
	
	@Override
	public boolean isApplicableType(){
		return this.type.isApplicableType();
	}
	
	public Type getBoundType(){
		Type t = this.type;
		while(t instanceof ForallType){
			t = ((ForallType)t).type;
		}
		return t;
	}

	@Override
	public int compareTo(Type o) {
		if(!(o instanceof ForallType)){
			return super.compareTo(o);
		}
		ForallType other = (ForallType)o;
		if(this.bound != other.bound){
			return this.bound.compareTo(other.bound);
		}
		return this.type.compareTo(other.type);
	}
}
