package expression;

import interpretation.Environment;
import types.Type;
import types.TypeConcrete;
import types.TypeRepresentation;

/**
 * Implementation of Binary Integer Literal
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class IntBinary extends LitInteger {
	/**
	 * Value of the integer literal
	 */
	public final int value;

	public IntBinary(int value) {
		this.value = value;
		this.setType(TypeConcrete.TypeInt);
	}

	@Override
	public Expression interpret(Environment env) {
		return this;
	}

	@Override
	public String toString() {
		return Integer.toString(this.value);
	}

	@Override
	public Type infer() throws Exception {
		return TypeConcrete.TypeInt;
	}

	@Override
	public String toClojureCode() throws Exception {
		return Integer.toString(this.value);
	}
	
	public static class ToIntRomanWrapper extends ConversionWrapper{

		public ToIntRomanWrapper(Expression wrapped) {
			super(wrapped);
		}

		@Override
		public Expression interpret(Environment env) throws Exception {
			Expression e = this.wrapped.interpret(env);
			if(!(e instanceof IntBinary)) {
				throw new Exception("Invalid wrapped conversion from IntString to IntBinary");
			}
			IntBinary i = (IntBinary)e;
			return new IntRoman(IntRoman.int2roman(i.value));
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
			return new ToIntRomanWrapper(this.wrapped.substituteTopLevelVariables(topLevel));
		}

		@Override
		public String toClojureCode() throws Exception {
			//TODO import
			return "(IntRoman/int2roman " + this.wrapped.toClojureCode() + ")";
		}
	}
	
	public static class ToIntStringWrapper extends ConversionWrapper{

		public ToIntStringWrapper(Expression wrapped) {
			super(wrapped);
		}

		@Override
		public Expression interpret(Environment env) throws Exception {
			Expression e = this.wrapped.interpret(env);
			if(!(e instanceof IntBinary)) {
				throw new Exception("Invalid wrapped conversion from IntBinary to IntString");
			}
			IntBinary i = (IntBinary)e;
			return new IntString(Integer.toString(i.value));
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
		public Expression substituteTopLevelVariables(Environment topLevel) {
			return new ToIntStringWrapper(this.wrapped.substituteTopLevelVariables(topLevel));
		}

		@Override
		public String toClojureCode() throws Exception {
			return "(Integer/toString " + this.wrapped.toClojureCode() + ")";
		}
		
	}
}
