package conversions;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeAtom;
import types.TypeTuple;
import util.AppendableException;
import util.Pair;
import expression.TypeConstructionLambda;

import java.util.Arrays;

import expression.Expression;
import expression.LitInteger;
import expression.LitString;
import expression.Literal;
import expression.Tuple;
import expression.Literal.ConversionWrapper;

public class IntStringToIntNativeWrapper extends ConversionWrapper {

	/**
	 * Private constructor to isolate the wrapper class
	 */
	private IntStringToIntNativeWrapper() {
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		Expression e = ConversionWrapper.arg.interpret(env);
		LitString s = (LitString) e;
		Literal l = new LitInteger(Integer.parseInt(s.value));
		return l;
	}

	@Override
	public String toClojureCode() throws AppendableException {
		return "(Integer/parseInt " + ConversionWrapper.arg.toClojureCode() + ")";
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return new Pair<Type, Substitution>(new TypeArrow(TypeAtom.TypeIntString, TypeAtom.TypeIntNative),
				Substitution.EMPTY);
	}
	
	@Override
	public String toString() {
		return "ConversionInternal:IntStringToIntBinary";
	}

	/**
	 * Conversion constructor from IntString to Int
	 */
	public static final TypeConstructionLambda IntStringToInt = new TypeConstructionLambda(TypeAtom.TypeIntNative,
			new Tuple(Arrays.asList(ConversionWrapper.arg)),
			new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)), new IntStringToIntNativeWrapper());
}