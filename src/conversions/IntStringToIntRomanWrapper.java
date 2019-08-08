package conversions;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeArrow;
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
		Expression e = ConversionWrapper.arg.interpret(env);
		if (!(e instanceof LitString)) {
			throw new AppendableException("Invalid wrapped conversion from IntString to IntRoman");
		}
		LitString s = (LitString) e;
		Literal l = new LitString(RomanNumbers.int2roman(Integer.parseInt(s.value)));
		return l;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return new Pair<Type, Substitution>(
				new TypeArrow(TypeRepresentation.TypeIntString, TypeRepresentation.TypeIntRoman), new Substitution());
	}

	@Override
	public String toClojureCode() throws AppendableException {
		return "(RomanNumbers/roman2int (Integer/parseInt " + ConversionWrapper.arg.toClojureCode() + "))";
	}

	/**
	 * Conversion constructor from IntString to IntRoman
	 */
	public static final TypeConstructionLambda IntStringToIntRoman = new TypeConstructionLambda(
			TypeRepresentation.TypeIntRoman, new Tuple(Arrays.asList(ConversionWrapper.arg)),
			new TypeTuple(Arrays.asList(TypeRepresentation.TypeIntString)), new IntStringToIntRomanWrapper());
}