package util;

import expression.Expression;
import types.TypeTuple;

public class ImplContainer  implements Comparable<ImplContainer>{
	public final TypeTuple typeSpec;
	public final Expression implementation;
	
	public ImplContainer(TypeTuple typeSpec, Expression implementation){
		this.typeSpec = typeSpec;
		this.implementation = implementation;
	}

	@Override
	public int compareTo(ImplContainer o) {
		return this.typeSpec.compareTo(o.typeSpec);
	}
	
	public boolean equals(Object o){
		if(!(o instanceof ImplContainer)){
			return false;
		}
		ImplContainer other = (ImplContainer)o;
		return typeSpec.equals(other.typeSpec);
	}
	
	public String toString(){
		return "I: " + this.typeSpec.toString() + " :: " + this.implementation.toString();
	}

}
