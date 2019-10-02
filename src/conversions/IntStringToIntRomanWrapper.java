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
import expression.Literal.ConversionWrapper;
import expression.Tuple;

/**
 * Hard coded wrapper for conversion from IntString to IntRoman
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class IntStringToIntRomanWrapper extends ConversionWrapper {

	/**
	 * Private constructor to isolate the wrapper class
	 */
	private IntStringToIntRomanWrapper() {
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		LitComposite e = (LitComposite) ConversionWrapper.arg.interpret(env);
		LitString s = (LitString) e.value.get(0);
		Literal l = new LitComposite(
				new Tuple(Arrays.asList(new LitString(RomanNumbers.int2roman(Integer.parseInt(s.value))))),
				TypeAtom.TypeIntRoman);
		return l;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return new Pair<Type, Substitution>(TypeAtom.TypeIntRoman, Substitution.EMPTY);
	}

	@Override
	public String toClojureCode() throws AppendableException {
		return "(RomanNumbers/roman2int (Integer/parseInt " + ConversionWrapper.arg.toClojureCode() + "))";
	}

	@Override
	public String toString() {
		return "ConversionInternal:IntStringToIntRoman";
	}

	/**
	 * Conversion constructor from IntString to IntRoman
	 */
	public static final Function IntStringToIntRoman = new Function(
			new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), new Tuple(Arrays.asList(ConversionWrapper.arg)),
			new IntStringToIntRomanWrapper(), Environment.topLevelEnvironment);
}