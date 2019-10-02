package conversions;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeAtom;
import types.TypeTuple;
import util.AppendableException;
import util.Pair;

import java.util.Arrays;

import expression.Expression;
import expression.Function;
import expression.LitComposite;
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
		LitComposite e = (LitComposite) ConversionWrapper.arg.interpret(env);
		LitString s = (LitString) e.value.get(0);
		Literal l = new LitInteger(Integer.parseInt(s.value));
		return l;
	}

	@Override
	public String toClojureCode() throws AppendableException {
		return "(Integer/parseInt " + ConversionWrapper.arg.toClojureCode() + ")";
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return new Pair<Type, Substitution>(TypeAtom.TypeIntNative, Substitution.EMPTY);
	}

	@Override
	public String toString() {
		return "ConversionInternal:IntStringToIntBinary";
	}

	/**
	 * Conversion constructor from IntString to Int
	 */
	public static final Function IntStringToInt = new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntString)),
			new Tuple(Arrays.asList(ConversionWrapper.arg)), new IntStringToIntNativeWrapper(),
			Environment.topLevelEnvironment);
}