package expression;

import interpretation.Environment;
import types.Type;
import types.TypeConcrete;

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
	}

	@Override
	public String toString() {
		return "- _x _y";
	}
	
	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		return this;
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
			IntBinary x = (IntBinary) env.getVariableValue(new Variable("_x"));
			IntBinary y = (IntBinary) env.getVariableValue(new Variable("_y"));

			if (x == null || y == null) {
				return this;
			}

			return new IntBinary(x.value - y.value); // TODO HOTFIX
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

	}
}
