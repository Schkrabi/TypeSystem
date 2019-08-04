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
import expression.LitString;
import expression.Literal;
import expression.Tuple;
import expression.Literal.ConversionWrapper;
import expression.LitInteger;

public class IntToIntRomanWrapper extends ConversionWrapper {

	/**
	 * Private constructor to isolate the wrapper class
	 */
	private IntToIntRomanWrapper() {
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression e = ConversionWrapper.arg.interpret(env);
		if (!(e instanceof LitInteger)) {
			throw new Exception("Invalid wrapped conversion from IntString to IntBinary");
		}
		LitInteger i = (LitInteger) e;
		Literal l = new LitString(RomanNumbers.int2roman(i.value));
		return l;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return new Pair<Type, Substitution>(new TypeArrow(TypeRepresentation.TypeInt, TypeRepresentation.TypeIntRoman),
				new Substitution());
	}

	@Override
	public String toClojureCode() throws Exception {
		return "(RomanNumbers/int2roman " + ConversionWrapper.arg.toClojureCode() + ")";
	}

	/**
	 * Conversion constructor from Int to IntRoman
	 */
	public static final TypeConstructionLambda IntToIntRoman = new TypeConstructionLambda(
			TypeRepresentation.TypeIntRoman, new Tuple(Arrays.asList(ConversionWrapper.arg)),
			new TypeTuple(Arrays.asList(TypeConcrete.TypeInt)), new IntToIntRomanWrapper());
}