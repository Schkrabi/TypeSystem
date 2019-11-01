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

public class IntRomanToIntStringWrapper extends ConversionWrapper {

	/**
	 * Private constructor to isolate the wrapper class
	 */
	private IntRomanToIntStringWrapper() {
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		LitComposite e = (LitComposite) ConversionWrapper.arg.interpret(env);
		LitString r = (LitString) e.value.get(0);
		Literal l = new LitComposite(
				new Tuple(Arrays.asList(new LitString(Integer.toString(RomanNumbers.roman2int(r.value))))),
				TypeAtom.TypeIntString);
		return l;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return new Pair<Type, Substitution>(TypeAtom.TypeIntString, Substitution.EMPTY);
	}

	@Override
	public String toClojureCode() throws AppendableException {
		return this.toClojureCode(TypeAtom.TypeIntString, Environment.topLevelEnvironment);
	}
	
	@Override
	protected String toClojureCode(Type expectedType, Environment env) throws AppendableException {
		return "(str (" + RomanNumbers.roman2intClojure + " " + ConversionWrapper.arg.toClojureCode() + "))";
	}

	@Override
	public String toString() {
		return "ConversionInternal:IntRomanToIntString";
	}

	/**
	 * Conversion constructor from IntRoman to IntString
	 */
	public static final Function IntRomanToIntString = new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntRoman)),
			new Tuple(Arrays.asList(ConversionWrapper.arg)), new IntRomanToIntStringWrapper(),
			Environment.topLevelEnvironment);
}