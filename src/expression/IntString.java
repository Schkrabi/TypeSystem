package expression;

import types.Type;
import types.TypeConcrete;
import types.TypeRepresentation;
import interpretation.Environment;

/**
 * Implementation of String Int Literal
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class IntString extends LitInteger {
	/**
	 * Value of the literal
	 */
	public final String value;

	/**
	 * Regular expression for string int value validation
	 */
	private static String intRegExp = "^-?([0-9]{1,9}|[0-1][0-9]{9}|20[0-9]{8}|21[0-3][0-9]{7}|214[0-6][0-9]{6}|2147[0-3][0-9]{5}|21474[0-7][0-9]{4}|214748[0-2][0-9]{3}|2147483[0-5][0-9]{2}|21474836[0-3][0-9]|214748364[0-7])$|^(-2147483648)$";

	public IntString(String value) {
		if (!value.matches(intRegExp)) {
			this.value = "";
			return;
		}
		this.value = value;
		this.setType(TypeRepresentation.TypeIntString);
	}

	@Override
	public Expression interpret(Environment env) {
		return this;
	}

	@Override
	public String toString() {
		return this.value;
	}

	@Override
	public Type infer() throws Exception {
		this.setType(TypeRepresentation.TypeIntString);
		return TypeRepresentation.TypeIntString;
	}

	@Override
	public String toClojureCode() throws Exception {
		return '"' + this.value + '"';
	}
	
	public static class ToIntBinaryWrapper extends ConversionWrapper{

		public ToIntBinaryWrapper(Expression wrapped) {
			super(wrapped);
		}

		@Override
		public Expression interpret(Environment env) throws Exception {
			Expression e = this.wrapped.interpret(env);
			if(!(e instanceof IntString)) {
				throw new Exception("Invalid wrapped conversion from IntString to IntBinary");
			}
			IntString s = (IntString)e;
			return new IntBinary(Integer.parseInt(s.value));
		}

		@Override
		public Expression substituteTopLevelVariables(Environment topLevel) {
			return new ToIntBinaryWrapper(this.wrapped.substituteTopLevelVariables(topLevel));
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
	
	public static class ToIntRomanWrapper extends ConversionWrapper{

		public ToIntRomanWrapper(Expression wrapped) {
			super(wrapped);
		}

		@Override
		public Expression interpret(Environment env) throws Exception {
			Expression e = this.wrapped.interpret(env);
			if(!(e instanceof IntString)) {
				throw new Exception("Invalid wrapped conversion from IntString to IntRoman");
			}
			IntString s = (IntString)e;
			return new IntRoman(IntRoman.int2roman(Integer.parseInt(s.value)));
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
		public Expression substituteTopLevelVariables(Environment topLevel) {
			return new ToIntRomanWrapper(this.wrapped.substituteTopLevelVariables(topLevel));
		}

		@Override
		public String toClojureCode() throws Exception {
			return "(IntRoman/roman2int (Integer/parseInt " + this.wrapped.toClojureCode() + "))";
		}
		
	}
}
