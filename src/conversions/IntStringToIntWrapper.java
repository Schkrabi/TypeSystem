package conversions;

import interpretation.Environment;
import types.Type;
import types.TypeConcrete;
import types.TypeRepresentation;
import types.TypeTuple;
import expression.Constructor;
import expression.Expression;
import expression.LitInteger;
import expression.LitString;
import expression.Literal;
import expression.Tuple;
import expression.Literal.ConversionWrapper;

public class IntStringToIntWrapper extends ConversionWrapper{

	/**
	 * Private constructor to isolate the wrapper class
	 */
	private IntStringToIntWrapper() {}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression e = ConversionWrapper.arg.interpret(env);
		if(!(e instanceof LitString)) {
			throw new Exception("Invalid wrapped conversion from IntString to IntBinary");
		}
		LitString s = (LitString)e;
		Literal l = new LitInteger(Integer.parseInt(s.value));
		l.setLiteralType(TypeConcrete.TypeInt);
		return l;
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		return this;
	}

	@Override
	public String toClojureCode() throws Exception {
		return "(Integer/parseInt " + ConversionWrapper.arg.toClojureCode() + ")";
	}

	@Override
	public Type infer() throws Exception {
		this.setType(TypeConcrete.TypeInt);
		return TypeConcrete.TypeInt;
	}
	
	/**
	 * Conversion constructor from IntString to Int
	 */
	public static final Constructor IntStringToInt = new Constructor(	TypeConcrete.TypeInt,
																			new Tuple(new Expression[]{ConversionWrapper.arg}),
																			new TypeTuple(new Type[]{TypeRepresentation.TypeIntString}),
																			new IntStringToIntWrapper());
}