package operators;

import interpretation.Environment;
import types.Type;
import types.TypeArrow;
import types.TypeConcrete;
import types.TypeTuple;
import expression.Expression;
import expression.Lambda;
import expression.LitInteger;
import expression.Tuple;
import expression.Variable;

/**
 * Wrapper class for Division lambda
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class Division extends Lambda {
	/**
	 * Wrapped addition lambda object
	 */
	public static final Division singleton = new Division();

	private Division() {
		super(new Tuple(
				new Variable[] { new Variable("_x"), new Variable("_y") }),
				DivWrapper.singleton);
	}

	@Override
	public String toString() {
		return "/";
	}
	
	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		return this;
	}
	
	@Override
	public Type infer(){
		Type t = new TypeArrow(new TypeTuple(new Type[]{TypeConcrete.TypeInt, TypeConcrete.TypeInt}), TypeConcrete.TypeInt);
		this.setType(t);
		return t;
	}
	
	@Override
	public String toClojureCode() throws Exception {
		return "/";
	}

	/**
	 * Private wrapper class for the body of division
	 * 
	 * @author Mgr. Radomir Skrabal
	 * 
	 */
	private static class DivWrapper extends Expression {
		/**
		 * The body expression object
		 */
		public static final DivWrapper singleton = new DivWrapper();

		private DivWrapper() {}

		@Override
		public Expression interpret(Environment env) throws Exception {
			LitInteger x = (LitInteger) (env.getVariableValue(new Variable("_x")).interpret(env));
			LitInteger y = (LitInteger) (env.getVariableValue(new Variable("_y")).interpret(env));

			if (x == null || y == null) {
				return this;
			}

			return new LitInteger(x.value / y.value);
		}

		@Override
		public Type infer() throws Exception {
			this.setType(TypeConcrete.TypeInt);
			return TypeConcrete.TypeInt;
		}

		@Override
		public Expression substituteTopLevelVariables(Environment topLevel) {
			return this;
		}

		@Override
		public String toClojureCode() throws Exception {
			throw new Exception("Use of DivWrapper for Clojure compilation is not supported.");
		}
	}
}
