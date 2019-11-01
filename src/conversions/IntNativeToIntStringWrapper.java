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
		Literal l = new LitComposite(new Tuple(Arrays.asList(new LitString(Integer.toString(i.value)))),
				TypeAtom.TypeIntString);
		return l;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return new Pair<Type, Substitution>(TypeAtom.TypeIntString, Substitution.EMPTY);
	}

	@Override
	public String toClojureCode() throws AppendableException {
		return this.toClojureCode(null, Environment.topLevelEnvironment);
	}
	
	@Override
	protected String toClojureCode(Type expectedType, Environment env) throws AppendableException {
		return "(Integer/toString " + ConversionWrapper.arg.toClojureCode() + ")";
	}

	@Override
	public String toString() {
		return "ConversionInternal:IntBinaryToIntString";
	}

	/**
	 * Conversion constructor from Int to IntString
	 */
	public static final Function IntToIntString = new Function(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative)),
			new Tuple(Arrays.asList(ConversionWrapper.arg)), new IntNativeToIntStringWrapper(),
			Environment.topLevelEnvironment);
}