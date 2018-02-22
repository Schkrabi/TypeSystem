package conversions;

import interpretation.Environment;
import types.Type;
import types.TypeConcrete;
import types.TypeRepresentation;
import util.RomanNumbers;
import expression.Expression;
import expression.LitInteger;
import expression.LitString;
import expression.Literal;
import expression.Literal.ConversionWrapper;

public class IntRomanToIntBinaryWrapper extends ConversionWrapper{
	
	public IntRomanToIntBinaryWrapper(Expression expr) {
		super(expr);
	}
	
	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression e = this.wrapped.interpret(env);
		if(!(e instanceof LitString)) {
			throw new Exception("Invalid wrapped conversion from IntRoman to IntBinary");
		}
		LitString r = (LitString)e;
		Literal l = new LitInteger(RomanNumbers.roman2int(r.value)); 
		l.setLiteralType(TypeConcrete.TypeInt);
		return l;
	}

	@Override
	public Type infer() throws Exception {
		Type t = this.wrapped.infer();
		if(t != TypeRepresentation.TypeIntRoman) {
			throw new Exception("Invalid wrapped conversion from IntRoman to IntBinary");
		}
		
		this.setType(TypeConcrete.TypeInt);
		return TypeConcrete.TypeInt;
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		return new IntRomanToIntBinaryWrapper(this.wrapped.substituteTopLevelVariables(topLevel));
	}

	@Override
	public String toClojureCode() throws Exception {
		//TODO import library
		return "(RomanNumbers/roman2int " + this.wrapped.toClojureCode() + ")";
	}
}