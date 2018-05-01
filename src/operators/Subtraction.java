package operators;

import expression.Expression;
import expression.Lambda;
import expression.LitInteger;
import expression.Tuple;
import expression.Variable;
import interpretation.Environment;
import types.Type;
import types.TypeArrow;
import types.TypeConcrete;
import types.TypeTuple;

/**
 * Wrapper class for subtraction lambda
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Subtraction extends Lambda {

	/**
	 * Wrapped subtraction lambda object
	 */
	public static final Subtraction singleton = new Subtraction();

	private Subtraction() {
		super(new Tuple(new Variable[] { new Variable("_x"), new Variable("_y") }), SubWrapper.singleton);
		this.infer(new Environment());
	}

	@Override
	public String toString() {
		return "-";
	}
	
	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		return this;
	}
	
	@Override
	public Type infer(Environment env){
		Type t = new TypeArrow(new TypeTuple(new Type[]{TypeConcrete.TypeInt, TypeConcrete.TypeInt}), TypeConcrete.TypeInt);
		this.setType(t);
		return t;
	}
	
	@Override
	public String toClojureCode() throws Exception {
		return "-";
	}

	/**
	 * Private wrapper class for the body of subtraction
	 * 
	 * @author Mgr. Radomir Skrabal
	 *
	 */
	private static class SubWrapper extends Expression {
		/**
		 * The body expression object
		 */
		public static final SubWrapper singleton = new SubWrapper();

		private SubWrapper() {
		}

		@Override
		public Expression interpret(Environment env) throws Exception {
			LitInteger x = (LitInteger) env.getVariableValue(new Variable("_x"));
			LitInteger y = (LitInteger) env.getVariableValue(new Variable("_y"));

			if (x == null || y == null) {
				return this;
			}

			return new LitInteger(x.value - y.value); // TODO HOTFIX
		}

		@Override
		public Type infer(Environment env) throws Exception {
			this.setType(TypeConcrete.TypeInt);
			return TypeConcrete.TypeInt;
		}

		@Override
		public Expression substituteTopLevelVariables(Environment topLevel) {
			return this;
		}

		@Override
		public String toClojureCode() throws Exception {
			throw new Exception("Use of SubWrapper for Clojure compilation is not supported.");
		}

	}
}
