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
import expression.Literal;
import expression.Tuple;
import expression.Literal.ConversionWrapper;
import expression.LitInteger;
import expression.LitString;

public class IntNativeToIntStringWrapper extends ConversionWrapper {

	/**
	 * Private constructor to isolate the wrapper class
	 */
	private IntNativeToIntStringWrapper() {
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		Expression e = ConversionWrapper.arg.interpret(env);
		LitInteger i = (LitInteger) e;
		Literal l = new LitString(Integer.toString(i.value));
		return l;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return new Pair<Type, Substitution>(new TypeArrow(TypeAtom.TypeIntNative, TypeAtom.TypeIntString),
				Substitution.EMPTY);
	}

	@Override
	public String toClojureCode() throws AppendableException {
		return "(Integer/toString " + ConversionWrapper.arg.toClojureCode() + ")";
	}
	
	@Override
	public String toString() {
		return "ConversionInternal:IntBinaryToIntString";
	}

	/**
	 * Conversion constructor from Int to IntString
	 */
	public static final TypeConstructionLambda IntToIntString = new TypeConstructionLambda(
			TypeAtom.TypeIntString, new Tuple(Arrays.asList(ConversionWrapper.arg)),
			new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)), new IntNativeToIntStringWrapper());
}