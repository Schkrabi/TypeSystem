package types;

import java.util.Set;
import java.util.TreeSet;

public class TypeVariable extends Type implements Comparable<TypeVariable>{
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

	@Override
	public int compareTo(TypeVariable arg) {
		return this.name.compareTo(arg.name);
	}

	@Override
	public Set<TypeVariable> getUnconstrainedVariables() {
		Set<TypeVariable> s = new TreeSet<TypeVariable>();
		if(this.getRep() == this){
			s.add(this);
			return s;
		}
		s.addAll(this.getRep().getUnconstrainedVariables());
		return s;
	}
}
