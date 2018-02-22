package conversions;

import interpretation.Environment;
import types.Type;
import types.TypeConcrete;
import types.TypeRepresentation;
import expression.Expression;
import expression.LitInteger;
import expression.LitString;
import expression.Literal;
import expression.Literal.ConversionWrapper;

public class IntStringToIntBinaryWrapper extends ConversionWrapper{

	public IntStringToIntBinaryWrapper(Expression wrapped) {
		super(wrapped);
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression e = this.wrapped.interpret(env);
		if(!(e instanceof LitString)) {
			throw new Exception("Invalid wrapped conversion from IntString to IntBinary");
		}
		LitString s = (LitString)e;
		Literal l = new LitInteger(Integer.parseInt(s.value));
		l.setLiteralType(TypeConcrete.TypeInt);
		return l;
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		return new IntStringToIntBinaryWrapper(this.wrapped.substituteTopLevelVariables(topLevel));
	}

	@Override
	public String toClojureCode() throws Exception {
		return "(Integer/parseInt " + this.wrapped.toClojureCode() + ")";
	}

	@Override
	public Type infer() throws Exception {
		Type t = this.wrapped.infer();
		if(t != TypeRepresentation.TypeIntString) {
			throw new Exception("Invalid wrapped conversion from IntString to IntBinary");
		}
		this.setType(TypeConcrete.TypeInt);
		return TypeConcrete.TypeInt;
	}
}