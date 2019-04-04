package conversions;

import interpretation.Environment;
import types.Type;
import types.TypeConcrete;
import types.TypeRepresentation;
import types.TypeTuple;
import util.RomanNumbers;
import expression.TypeConstructionLambda;
import expression.Expression;
import expression.LitString;
import expression.Literal;
import expression.Tuple;
import expression.Literal.ConversionWrapper;

public class IntRomanToIntStringWrapper extends ConversionWrapper{
	
	/**
	 * Private constructor to isolate the wrapper class
	 */
	private IntRomanToIntStringWrapper() {}
	
	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression e = ConversionWrapper.arg.interpret(env);
		if(!(e instanceof LitString)) {
			throw new Exception("Invalid wrapped conversion from IntRoman to IntString");
		}
		LitString r = (LitString)e;
		Literal l = new LitString(Integer.toString(RomanNumbers.roman2int(r.value))); 
		l.setLiteralType(TypeRepresentation.TypeIntString);
		return l;
	}

	@Override
	public Type infer(Environment env) {
		this.setType(TypeConcrete.TypeInt);
		return TypeRepresentation.TypeIntString;
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		return this;
	}

	@Override
	public String toClojureCode() throws Exception {
		return "(Integer/toString (RomanNumbers/roman2int " + ConversionWrapper.arg.toClojureCode() + "))";
	}
	
	/**
	 * Conversion constructor from IntRoman to IntString
	 */
	public static final TypeConstructionLambda IntRomanToIntString = new TypeConstructionLambda(	TypeRepresentation.TypeIntString,
																			new Tuple(new Expression[]{ConversionWrapper.arg}),
																			new TypeTuple(new Type[]{TypeRepresentation.TypeIntRoman}),
																			new IntRomanToIntStringWrapper());
}