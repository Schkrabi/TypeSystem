package conversions;

import interpretation.Environment;
import types.Type;
import types.TypeConcrete;
import types.TypeRepresentation;
import util.RomanNumbers;
import expression.Expression;
import expression.LitString;
import expression.Literal;
import expression.Literal.ConversionWrapper;
import expression.LitInteger;

public class IntBinaryToIntRomanWrapper extends ConversionWrapper{

	public IntBinaryToIntRomanWrapper(Expression wrapped) {
		super(wrapped);
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression e = this.wrapped.interpret(env);
		if(!(e instanceof LitInteger)) {
			throw new Exception("Invalid wrapped conversion from IntString to IntBinary");
		}
		LitInteger i = (LitInteger)e;
		Literal l = new LitString(RomanNumbers.int2roman(i.value));
		l.setLiteralType(TypeRepresentation.TypeIntRoman);
		return l;
	}

	@Override
	public Type infer() throws Exception {
		Type t = this.wrapped.infer();
		if(t != TypeConcrete.TypeInt) {
			throw new Exception("Invalid wrapped conversion from IntBinary to IntRoman");
		}
		this.setType(TypeRepresentation.TypeIntRoman);
		return TypeRepresentation.TypeIntRoman;
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		return new IntBinaryToIntRomanWrapper(this.wrapped.substituteTopLevelVariables(topLevel));
	}

	@Override
	public String toClojureCode() throws Exception {
		//TODO import
		return "(RomanNumbers/int2roman " + this.wrapped.toClojureCode() + ")";
	}
}