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

public class IntRomanToIntStringWrapper extends ConversionWrapper{
	
	public IntRomanToIntStringWrapper(Expression expr) {
		super(expr);
	}
	
	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression e = this.wrapped.interpret(env);
		if(!(e instanceof LitString)) {
			throw new Exception("Invalid wrapped conversion from IntRoman to IntString");
		}
		LitString r = (LitString)e;
		Literal l = new LitString(Integer.toString(RomanNumbers.roman2int(r.value))); 
		l.setLiteralType(TypeRepresentation.TypeIntString);
		return l;
	}

	@Override
	public Type infer() throws Exception {
		Type t = this.wrapped.infer();
		if(t != TypeRepresentation.TypeIntRoman) {
			throw new Exception("Invalid wrapped conversion from IntRoman to IntString");
		}
		
		this.setType(TypeConcrete.TypeInt);
		return TypeRepresentation.TypeIntString;
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		return new IntRomanToIntStringWrapper(this.wrapped.substituteTopLevelVariables(topLevel));
	}

	@Override
	public String toClojureCode() throws Exception {
		return "(Integer/toString (RomanNumbers/roman2int " + this.wrapped.toClojureCode() + "))";
	}
}