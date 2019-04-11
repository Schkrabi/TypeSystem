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
	public Expression interpret(Environment env) throws Exception {
		Expression e = ConversionWrapper.arg.interpret(env);
		if(!(e instanceof LitInteger)) {
			throw new Exception("Invalid wrapped conversion from IntBinary to IntString");
		}
		LitInteger i = (LitInteger)e;
		Literal l = new LitString(Integer.toString(i.value));
		return l;
	}

	@Override
	public Map<Expression, Type> infer(Environment env) throws AppendableException {
		try {
			Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
			if (this.typeHypothesis == null) {
				this.typeHypothesis = ConversionWrapper.arg.infer(env);
				this.typeHypothesis.put(this, TypeRepresentation.TypeIntString);
			}
			hyp.putAll(this.typeHypothesis);
			return hyp;
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		return this;
	}

	@Override
	public String toClojureCode() throws Exception {
		return "(Integer/toString " + ConversionWrapper.arg.toClojureCode() + ")";
	}
	
	/**
	 * Conversion constructor from Int to IntString
	 */
	public static final TypeConstructionLambda IntToIntString = new TypeConstructionLambda(	TypeRepresentation.TypeIntString,
																			new Tuple(new Expression[]{ConversionWrapper.arg}),
																			new TypeTuple(new Type[]{TypeConcrete.TypeInt}),
																			new IntToIntStringWrapper());
}