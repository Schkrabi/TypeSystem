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
import expression.Literal;
import expression.Tuple;
import expression.Literal.ConversionWrapper;
import expression.LitInteger;
import expression.LitString;

public class IntToIntStringWrapper extends ConversionWrapper{

	/**
	 * Private constructor to isolate the wrapper class
	 */
	private IntToIntStringWrapper() {}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		Expression e = ConversionWrapper.arg.interpret(env);
		if(!(e instanceof LitInteger)) {
			throw new AppendableException("Invalid wrapped conversion from IntBinary to IntString");
		}
		LitInteger i = (LitInteger)e;
		Literal l = new LitString(Integer.toString(i.value));
		return l;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return new Pair<Type, Substitution>(new TypeArrow(TypeRepresentation.TypeInt, TypeRepresentation.TypeIntString), new Substitution());
	}
	
	@Override
	public String toClojureCode() throws AppendableException {
		return "(Integer/toString " + ConversionWrapper.arg.toClojureCode() + ")";
	}
	
	/**
	 * Conversion constructor from Int to IntString
	 */
	public static final TypeConstructionLambda IntToIntString = new TypeConstructionLambda(	TypeRepresentation.TypeIntString,
																			new Tuple(Arrays.asList(ConversionWrapper.arg)),
																			new TypeTuple(Arrays.asList(TypeConcrete.TypeInt)),
																			new IntToIntStringWrapper());
}