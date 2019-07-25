package conversions;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeConcrete;
import types.TypeRepresentation;
import types.TypeTuple;
import util.AppendableException;
import util.Pair;
import util.RomanNumbers;
import expression.TypeConstructionLambda;

import expression.Expression;
import expression.LitInteger;
import expression.LitString;
import expression.Literal;
import expression.Tuple;
import expression.Literal.ConversionWrapper;

public class IntRomanToIntWrapper extends ConversionWrapper{
	
	/**
	 * Private constructor to isolate the wrapper class
	 */
	private IntRomanToIntWrapper() {}
	
	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression e = ConversionWrapper.arg.interpret(env);
		if(!(e instanceof LitString)) {
			throw new Exception("Invalid wrapped conversion from IntRoman to IntBinary");
		}
		LitString r = (LitString)e;
		Literal l = new LitInteger(RomanNumbers.roman2int(r.value)); 
		return l;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return new Pair<Type, Substitution>(new TypeArrow(TypeRepresentation.TypeIntRoman, TypeRepresentation.TypeInt), new Substitution());
	}

	@Override
	public String toClojureCode() throws Exception {
		return "(RomanNumbers/roman2int " + ConversionWrapper.arg.toClojureCode() + ")";
	}
	
	/**
	 * Conversion constructor from IntRoman to Int
	 */
	public static final TypeConstructionLambda IntRomanToInt = new TypeConstructionLambda(	TypeConcrete.TypeInt,
																			new Tuple(new Expression[]{ConversionWrapper.arg}),
																			new TypeTuple(new Type[]{TypeRepresentation.TypeIntRoman}),
																			new IntRomanToIntWrapper());
}