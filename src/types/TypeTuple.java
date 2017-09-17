package types;

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
}
