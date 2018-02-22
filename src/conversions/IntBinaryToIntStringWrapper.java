package conversions;

import interpretation.Environment;
import types.Type;
import types.TypeConcrete;
import types.TypeRepresentation;
import expression.Expression;
import expression.Literal;
import expression.Literal.ConversionWrapper;
import expression.LitInteger;
import expression.LitString;

public class IntBinaryToIntStringWrapper extends ConversionWrapper{

	public IntBinaryToIntStringWrapper(Expression wrapped) {
		super(wrapped);
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression e = this.wrapped.interpret(env);
		if(!(e instanceof LitInteger)) {
			throw new Exception("Invalid wrapped conversion from IntBinary to IntString");
		}
		LitInteger i = (LitInteger)e;
		Literal l = new LitString(Integer.toString(i.value));
		l.setLiteralType(TypeRepresentation.TypeIntString);
		return l;
	}

	@Override
	public Type infer() throws Exception {
		Type t = this.wrapped.infer();
		if(t != TypeConcrete.TypeInt) {
			throw new Exception("Invalid wrapped conversion from IntBinary to IntRoman");
		}
		this.setType(TypeRepresentation.TypeIntString);
		return TypeRepresentation.TypeIntString;
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		return new IntBinaryToIntStringWrapper(this.wrapped.substituteTopLevelVariables(topLevel));
	}

	@Override
	public String toClojureCode() throws Exception {
		return "(Integer/toString " + this.wrapped.toClojureCode() + ")";
	}
	
}