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

public class IntRomanToIntStringWrapper extends ConversionWrapper {

	/**
	 * Private constructor to isolate the wrapper class
	 */
	private IntRomanToIntStringWrapper() {
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		Expression e = ConversionWrapper.arg.interpret(env);
		LitString r = (LitString) e;
		Literal l = new LitString(Integer.toString(RomanNumbers.roman2int(r.value)));
		return l;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return new Pair<Type, Substitution>(
				new TypeArrow(TypeAtom.TypeIntRoman, TypeAtom.TypeIntString), Substitution.EMPTY);
	}

	@Override
	public String toClojureCode() throws AppendableException {
		//TODO rewrite!
		return "(Integer/toString (RomanNumbers/roman2int " + ConversionWrapper.arg.toClojureCode() + "))";
	}
	
	@Override
	public String toString() {
		return "ConversionInternal:IntRomanToIntString";
	}

	/**
	 * Conversion constructor from IntRoman to IntString
	 */
	public static final TypeConstructionLambda IntRomanToIntString = new TypeConstructionLambda(
			TypeAtom.TypeIntString, new Tuple(Arrays.asList(ConversionWrapper.arg)),
			new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)), new IntRomanToIntStringWrapper());
}