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
import expression.LitInteger;
import expression.LitString;
import expression.Literal;
import expression.Tuple;
import expression.Literal.ConversionWrapper;

public class IntRomanToIntNativeWrapper extends ConversionWrapper {

	/**
	 * Private constructor to isolate the wrapper class
	 */
	private IntRomanToIntNativeWrapper() {
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		Expression e = ConversionWrapper.arg.interpret(env);
		LitString r = (LitString) e;
		Literal l = new LitInteger(RomanNumbers.roman2int(r.value));
		return l;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return new Pair<Type, Substitution>(new TypeArrow(TypeAtom.TypeIntRoman, TypeAtom.TypeIntNative),
				Substitution.EMPTY);
	}

	@Override
	public String toClojureCode() throws AppendableException {
		return "(RomanNumbers/roman2int " + ConversionWrapper.arg.toClojureCode() + ")";
	}

	@Override
	public String toString() {
		return "ConversionInternal:IntRomanToIntBinary";
	}

	/**
	 * Conversion constructor from IntRoman to Int
	 */
	public static final TypeConstructionLambda IntRomanToInt = new TypeConstructionLambda(TypeAtom.TypeIntNative,
			new Tuple(Arrays.asList(ConversionWrapper.arg)), new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)),
			new IntRomanToIntNativeWrapper());
}