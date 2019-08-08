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
import expression.TypeConstructionLambda;

import java.util.Arrays;

import expression.Expression;
import expression.LitInteger;
import expression.LitString;
import expression.Literal;
import expression.Tuple;
import expression.Literal.ConversionWrapper;

public class IntStringToIntWrapper extends ConversionWrapper {

	/**
	 * Private constructor to isolate the wrapper class
	 */
	private IntStringToIntWrapper() {
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		Expression e = ConversionWrapper.arg.interpret(env);
		if (!(e instanceof LitString)) {
			throw new AppendableException("Invalid wrapped conversion from IntString to IntBinary");
		}
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
		return new Pair<Type, Substitution>(new TypeArrow(TypeRepresentation.TypeIntString, TypeRepresentation.TypeInt),
				new Substitution());
	}

	/**
	 * Conversion constructor from IntString to Int
	 */
	public static final TypeConstructionLambda IntStringToInt = new TypeConstructionLambda(TypeConcrete.TypeInt,
			new Tuple(Arrays.asList(ConversionWrapper.arg)),
			new TypeTuple(Arrays.asList(TypeRepresentation.TypeIntString)), new IntStringToIntWrapper());
}