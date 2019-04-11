package conversions;

import interpretation.Environment;
import types.Type;
import types.TypeConcrete;
import types.TypeRepresentation;
import types.TypeTuple;
import util.AppendableException;
import expression.TypeConstructionLambda;

import java.util.Map;
import java.util.TreeMap;

import expression.Expression;
import expression.LitInteger;
import expression.LitString;
import expression.Literal;
import expression.Tuple;
import expression.Literal.ConversionWrapper;

public class IntStringToIntWrapper extends ConversionWrapper{

	/**
	 * Private constructor to isolate the wrapper class
	 */
	private IntStringToIntWrapper() {}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression e = ConversionWrapper.arg.interpret(env);
		if(!(e instanceof LitString)) {
			throw new Exception("Invalid wrapped conversion from IntString to IntBinary");
		}
		LitString s = (LitString)e;
		Literal l = new LitInteger(Integer.parseInt(s.value));
		return l;
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		return this;
	}

	@Override
	public String toClojureCode() throws Exception {
		return "(Integer/parseInt " + ConversionWrapper.arg.toClojureCode() + ")";
	}

	@Override
	public Map<Expression, Type> infer(Environment env) throws AppendableException {
		try {
			Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
			if (this.typeHypothesis == null) {
				this.typeHypothesis = ConversionWrapper.arg.infer(env);
				this.typeHypothesis.put(this, TypeConcrete.TypeInt);
			}
			hyp.putAll(this.typeHypothesis);
			return hyp;
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}
	
	/**
	 * Conversion constructor from IntString to Int
	 */
	public static final TypeConstructionLambda IntStringToInt = new TypeConstructionLambda(	TypeConcrete.TypeInt,
																			new Tuple(new Expression[]{ConversionWrapper.arg}),
																			new TypeTuple(new Type[]{TypeRepresentation.TypeIntString}),
																			new IntStringToIntWrapper());
}