package conversions;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeAtom;
import types.TypeTuple;
import util.AppendableException;
import util.Pair;
import util.RomanNumbers;

import java.util.Arrays;

import expression.Expression;
import expression.Function;
import expression.LitComposite;
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
		Literal l = new LitComposite(new Tuple(Arrays.asList(new LitString(RomanNumbers.int2roman(i.value)))),
				TypeAtom.TypeIntRoman);
		return l;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return new Pair<Type, Substitution>(TypeAtom.TypeIntRoman, Substitution.EMPTY);
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
	public static final Function IntToIntRoman = new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
			new Tuple(Arrays.asList(ConversionWrapper.arg)), new IntNativeToIntRomanWrapper(),
			Environment.topLevelEnvironment);
}