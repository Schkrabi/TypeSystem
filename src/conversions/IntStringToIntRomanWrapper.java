package conversions;

import interpretation.Environment;
import types.Type;
import types.TypeRepresentation;
import util.RomanNumbers;
import expression.Expression;
import expression.LitString;
import expression.Literal;
import expression.Literal.ConversionWrapper;

public class IntStringToIntRomanWrapper extends ConversionWrapper{

	public IntStringToIntRomanWrapper(Expression wrapped) {
		super(wrapped);
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression e = this.wrapped.interpret(env);
		if(!(e instanceof LitString)) {
			throw new Exception("Invalid wrapped conversion from IntString to IntRoman");
		}
		LitString s = (LitString)e;
		Literal l = new LitString(RomanNumbers.int2roman(Integer.parseInt(s.value)));
		l.setLiteralType(TypeRepresentation.TypeIntRoman);
		return l;
	}

	@Override
	public Type infer() throws Exception {
		Type t = this.wrapped.infer();
		if(t != TypeRepresentation.TypeIntString) {
			throw new Exception("Invalid wrapped conversion from IntString to IntRoman");
		}
		this.setType(TypeRepresentation.TypeIntRoman);
		return TypeRepresentation.TypeIntRoman;
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		return new IntStringToIntRomanWrapper(this.wrapped.substituteTopLevelVariables(topLevel));
	}

	@Override
	public String toClojureCode() throws Exception {
		return "(RomanNumbers/roman2int (Integer/parseInt " + this.wrapped.toClojureCode() + "))";
	}
	
}