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
		LitComposite e = (LitComposite) ConversionWrapper.arg.interpret(env);
		LitString r = (LitString) e.value.get(0);
		Literal l = new LitInteger(RomanNumbers.roman2int(r.value));
		return l;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return new Pair<Type, Substitution>(TypeAtom.TypeIntNative, Substitution.EMPTY);
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
	public static final Function IntRomanToInt = new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)),
			new Tuple(Arrays.asList(ConversionWrapper.arg)), new IntRomanToIntNativeWrapper(),
			Environment.topLevelEnvironment);
}