package expression;

import types.Type;
import types.TypeArrow;
import types.TypeConcrete;
import types.TypeTuple;

public class Subtraction extends Lambda {
	public Subtraction() {
		super(new Tuple(new Variable[]{new Variable("_x"), new Variable("_y")}), Sub.singleton);
	}
	
	@Override
	public Type infer(){
		return new TypeArrow(new TypeTuple(new Type[]{TypeConcrete.TypeInt, TypeConcrete.TypeInt}), TypeConcrete.TypeInt);
	}
}
