package expression;

import types.Type;
import types.TypeConcrete;
import interpretation.Environment;

/**
 * Wrapper class for addition lambda
 * 
 * @author schkrabi
 * 
 */
public class Addition extends Lambda {

	/**
	 * Wrapped addition lambda object
	 */
	public static final Addition singleton = new Addition();

	private Addition() {
		super(new Tuple(
				new Variable[] { new Variable("_x"), new Variable("_y") }),
				AddWrapper.singleton);
	}

	@Override
	public String toString() {
		return "+ _x _y";
	}

	/**
	 * Private wrapper class for the body of addition
	 * 
	 * @author schkrabi
	 * 
	 */
	private static class AddWrapper extends Expression {
		/**
		 * The body object
		 */
		public static final AddWrapper singleton = new AddWrapper();

		private AddWrapper() {
		}

		@Override
		public Expression interpret(Environment env) throws Exception {
			IntBinary x = (IntBinary) env.getVariableValue(new Variable("_x"));
			IntBinary y = (IntBinary) env.getVariableValue(new Variable("_y"));

			if (x == null || y == null) {
				return this;
			}

			return new IntBinary(x.value + y.value); // TODO HOTFIX
		}

		@Override
		public Type infer() throws Exception {
			this.setType(TypeConcrete.TypeIntBinary);
			return TypeConcrete.TypeIntBinary;
		}
	}
}
