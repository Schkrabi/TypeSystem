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

import java.util.Arrays;

import expression.Expression;
import expression.LitInteger;
import expression.LitString;
import expression.Literal;
import expression.Tuple;
import expression.Literal.ConversionWrapper;

public class IntRomanToIntWrapper extends ConversionWrapper {

	/**
	 * Private constructor to isolate the wrapper class
	 */
	private IntRomanToIntWrapper() {
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		Expression e = ConversionWrapper.arg.interpret(env);
		if (!(e instanceof LitString)) {
			throw new AppendableException("Invalid wrapped conversion from IntRoman to IntBinary");
		}
		LitString r = (LitString) e;
		Literal l = new LitInteger(RomanNumbers.roman2int(r.value));
		return l;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return new Pair<Type, Substitution>(new TypeArrow(TypeRepresentation.TypeIntRoman, TypeRepresentation.TypeInt),
				new Substitution());
	}

	@Override
	public String toClojureCode() throws AppendableException {
		return "(RomanNumbers/roman2int " + ConversionWrapper.arg.toClojureCode() + ")";
	}

	/**
	 * Conversion constructor from IntRoman to Int
	 */
	public static final TypeConstructionLambda IntRomanToInt = new TypeConstructionLambda(TypeConcrete.TypeInt,
			new Tuple(Arrays.asList(ConversionWrapper.arg)),
			new TypeTuple(Arrays.asList(TypeRepresentation.TypeIntRoman)), new IntRomanToIntWrapper());
}