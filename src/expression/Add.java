package expression;

import types.Type;
import types.TypeArrow;
import types.TypeConcrete;
import types.TypeTuple;
import interpretation.Environment;

/** @deprecated */
public class Add extends Expression {

	private Add() {
	}

	public static final Add singleton = new Add();

	@Override
	public Expression interpret(Environment env) throws Exception {
		IntBinary x = (IntBinary) env.getVariableValue(new Variable("_x"));
		IntBinary y = (IntBinary) env.getVariableValue(new Variable("_y"));

		if (x == null || y == null) {
			return this;
		}

		return new IntBinary(x.value + y.value);
	}

	@Override
	public String toString() {
		return "+ _x _y";
	}

	@Override
	public Type infer() throws Exception {
		return new TypeArrow(new TypeTuple(new Type[] { TypeConcrete.TypeInt, TypeConcrete.TypeInt }),
				TypeConcrete.TypeInt);
	}

}
