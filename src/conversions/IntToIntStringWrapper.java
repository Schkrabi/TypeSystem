package conversions;

import interpretation.Environment;
import types.Type;
import types.TypeConcrete;
import types.TypeRepresentation;
import types.TypeTuple;
import expression.TypeConstructionLambda;
import expression.Expression;
import expression.Literal;
import expression.Tuple;
import expression.Literal.ConversionWrapper;
import expression.LitInteger;
import expression.LitString;

public class IntToIntStringWrapper extends ConversionWrapper{

	/**
	 * Private constructor to isolate the wrapper class
	 */
	private IntToIntStringWrapper() {}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression e = ConversionWrapper.arg.interpret(env);
		if(!(e instanceof LitInteger)) {
			throw new Exception("Invalid wrapped conversion from IntBinary to IntString");
		}
		LitInteger i = (LitInteger)e;
		Literal l = new LitString(Integer.toString(i.value));
		l.setLiteralType(TypeRepresentation.TypeIntString);
		return l;
	}

	@Override
	public Type infer() throws Exception {
		this.setType(TypeRepresentation.TypeIntString);
		return TypeRepresentation.TypeIntString;
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		return this;
	}

	@Override
	public String toClojureCode() throws Exception {
		return "(Integer/toString " + ConversionWrapper.arg.toClojureCode() + ")";
	}
	
	/**
	 * Conversion constructor from Int to IntString
	 */
	public static final TypeConstructionLambda IntToIntString = new TypeConstructionLambda(	TypeRepresentation.TypeIntString,
																			new Tuple(new Expression[]{ConversionWrapper.arg}),
																			new TypeTuple(new Type[]{TypeConcrete.TypeInt}),
																			new IntToIntStringWrapper());
}