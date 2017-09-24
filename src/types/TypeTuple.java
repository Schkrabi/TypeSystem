package types;

import java.util.Set;
import java.util.TreeSet;

public class TypeTuple extends Type {
	
	public final Type[] values;
	
	public TypeTuple(Type[] values){
		this.values = values;
	}

	@Override
	public String toString(){
		StringBuilder s = new StringBuilder("[");
		for(int i = 0; i < this.values.length; i++){
			s.append(this.values[i].toString());
			if(i != this.values.length - 1){
				s.append(", ");
			}
		}
		s.append("]");
		return s.toString();
	}
	
	@Override
	public boolean equals(Object o){
		if(!(o instanceof TypeTuple)){
			return false;
		}
		TypeTuple other = (TypeTuple)o;
		if(this.values.length != other.values.length){
			return false;
		}
		
		for(int i = 0; i < this.values.length; i++){
			if(!this.values[i].equals(other.values[i])){
				return false;
			}
		}
		return true;
	}

	@Override
	public Set<TypeVariable> getUnconstrainedVariables() {
		Set<TypeVariable> s = new TreeSet<TypeVariable>();
		if(this.getRep() != this){
			s.addAll(this.getRep().getUnconstrainedVariables());
			return s;
		}
		
		for(int i = 0; i < this.values.length; i++){
			s.addAll(this.values[i].getUnconstrainedVariables());
		}
		return s;
	}
}
