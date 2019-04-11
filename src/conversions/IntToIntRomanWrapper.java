package conversions;

import interpretation.Environment;
import types.Type;
import types.TypeConcrete;
import types.TypeRepresentation;
import types.TypeTuple;
import util.AppendableException;
import util.RomanNumbers;
import expression.TypeConstructionLambda;

import java.util.Map;
import java.util.TreeMap;

import expression.Expression;
import expression.LitString;
import expression.Literal;
import expression.Tuple;
import expression.Literal.ConversionWrapper;
import expression.LitInteger;

public class IntToIntRomanWrapper extends ConversionWrapper{

	/**
	 * Private constructor to isolate the wrapper class
	 */
	private IntToIntRomanWrapper() {}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression e = ConversionWrapper.arg.interpret(env);
		if(!(e instanceof LitInteger)) {
			throw new Exception("Invalid wrapped conversion from IntString to IntBinary");
		}
		LitInteger i = (LitInteger)e;
		Literal l = new LitString(RomanNumbers.int2roman(i.value));
		return l;
	}

	@Override
	public Map<Expression, Type> infer(Environment env) throws AppendableException {
		try {
			Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
			if (this.typeHypothesis == null) {
				this.typeHypothesis = ConversionWrapper.arg.infer(env);
				this.typeHypothesis.put(this, TypeRepresentation.TypeIntRoman);
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
		return "(RomanNumbers/int2roman " + ConversionWrapper.arg.toClojureCode() + ")";
	}
	
	/**
	 * Conversion constructor from Int to IntRoman
	 */
	public static final TypeConstructionLambda IntToIntRoman = new TypeConstructionLambda(	TypeRepresentation.TypeIntRoman,
																			new Tuple(new Expression[]{ConversionWrapper.arg}),
																			new TypeTuple(new Type[]{TypeConcrete.TypeInt}),
																			new IntToIntRomanWrapper());
}