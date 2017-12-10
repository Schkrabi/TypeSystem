package expression;

import types.Type;
import types.TypeConcrete;
import types.TypeArrow;
import types.TypeTuple;
import interpretation.Environment;

/**
 * Wrapper class for addition lambda
 * 
 * @author Mgr. Radomir Skrabal
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
		return "(+ _x _y)";
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
		return "(fn [x y] (+ x y))";
	}

	/**
	 * Private wrapper class for the body of addition
	 * 
	 * @author Mgr. Radomir Skrabal
	 * 
	 */
	private static class AddWrapper extends Expression {
		/**
		 * The body expression object
		 */
		public static final AddWrapper singleton = new AddWrapper();

		private AddWrapper() {
		}

		@Override
		public Expression interpret(Environment env) throws Exception {
			IntBinary x = (IntBinary) (env.getVariableValue(new Variable("_x")).interpret(env));
			IntBinary y = (IntBinary) (env.getVariableValue(new Variable("_y")).interpret(env));

			if (x == null || y == null) {
				return this;
			}

			return new IntBinary(x.value + y.value);
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
			throw new Exception("Use of AddWrapper for Clojure compilation is not supported.");
		}
	}
}
