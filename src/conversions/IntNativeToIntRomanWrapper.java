package conversions;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeAtom;
import types.TypeTuple;
import util.AppendableException;
import util.Pair;
import util.RomanNumbers;
import expression.TypeConstructionLambda;

import java.util.Arrays;

import expression.Expression;
import expression.LitString;
import expression.Literal;
import expression.Tuple;
import expression.Literal.ConversionWrapper;
import expression.LitInteger;

public class IntNativeToIntRomanWrapper extends ConversionWrapper {

	/**
	 * Private constructor to isolate the wrapper class
	 */
	private IntNativeToIntRomanWrapper() {
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		Expression e = ConversionWrapper.arg.interpret(env);
		LitInteger i = (LitInteger) e;
		Literal l = new LitString(RomanNumbers.int2roman(i.value));
		return l;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return new Pair<Type, Substitution>(new TypeArrow(TypeAtom.TypeIntNative, TypeAtom.TypeIntRoman),
				Substitution.EMPTY);
	}

	@Override
	public String toClojureCode() throws AppendableException {
		return "(RomanNumbers/int2roman " + ConversionWrapper.arg.toClojureCode() + ")";
	}
	
	@Override
	public String toString() {
		return "ConversionInternal:IntBinaryToIntRoman";
	}

	/**
	 * Conversion constructor from Int to IntRoman
	 */
	public static final TypeConstructionLambda IntToIntRoman = new TypeConstructionLambda(
			TypeAtom.TypeIntRoman, new Tuple(Arrays.asList(ConversionWrapper.arg)),
			new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), new IntNativeToIntRomanWrapper());
}