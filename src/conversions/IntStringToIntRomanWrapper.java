package conversions;

import interpretation.Environment;
import types.Type;
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
	public Expression interpret(Environment env) throws Exception {
		Expression e = ConversionWrapper.arg.interpret(env);
		if (!(e instanceof LitString)) {
			throw new Exception("Invalid wrapped conversion from IntString to IntRoman");
		}
		LitString s = (LitString) e;
		Literal l = new LitString(RomanNumbers.int2roman(Integer.parseInt(s.value)));
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
		return "(RomanNumbers/roman2int (Integer/parseInt " + ConversionWrapper.arg.toClojureCode() + "))";
	}

	/**
	 * Conversion constructor from IntString to IntRoman
	 */
	public static final TypeConstructionLambda IntStringToIntRoman = new TypeConstructionLambda(
			TypeRepresentation.TypeIntRoman, new Tuple(new Expression[] { ConversionWrapper.arg }),
			new TypeTuple(new Type[] { TypeRepresentation.TypeIntString }), new IntStringToIntRomanWrapper());
}