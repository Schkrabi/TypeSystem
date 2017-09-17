package expression;

import types.Type;
import types.TypeTuple;
import interpretation.Environment;

public class Tuple extends Expression {
	
	public final Expression[] values;
	
	public Tuple(Expression[] values){
		this.values = new Expression[values.length];
		for(int i = 0; i < values.length; i++){
			this.values[i] = values[i];
		}
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		return this; //Should tuple intepret to itself? Probably yes due to lazyness
	}

	@Override
	public String toString(){
		StringBuilder s = new StringBuilder("[");
		for(int i = 0; i < this.values.length; i++){
			s.append(this.values[i].toString());
			if(i + 1 < this.values.length){
				s.append(", ");
			}
		}
		s.append("]");
		return s.toString();
	}

	@Override
	public Type infer() throws Exception {
		Type types[] = new Type[this.values.length];
		for(int i = 0; i < this.values.length; i++){
			types[i] = this.values[i].infer();
		}
		return new TypeTuple(types);
	}
}
