package operators;

import interpretation.Environment;
import types.Type;
import types.TypeArrow;
import types.TypeConcrete;
import types.TypeTuple;
import expression.Expression;
import expression.Function;
import expression.LitBoolean;
import expression.Tuple;
import expression.Variable;

/**
 * Wrapper class for And lambda
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class Or extends Function {
	/**
	 * Wrapped addition lambda object
	 */
	public static final Or singleton = new Or();

	private Or() {
		super(	new TypeTuple(new Type[] { TypeConcrete.TypeBool, TypeConcrete.TypeBool}),
				new Tuple(new Variable[] { new Variable("_x"), new Variable("_y") }),
				OrWrapper.singleton,
				new Environment());
		this.infer(new Environment());
	}

	@Override
	public String toString() {
		return "OR";
	}
	
	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		return this;
	}
	
	@Override
	public Type infer(Environment env){
		Type t = new TypeArrow(new TypeTuple(new Type[]{TypeConcrete.TypeBool, TypeConcrete.TypeBool}), TypeConcrete.TypeBool);
		this.setType(t);
		return t;
	}
	
	@Override
	public String toClojureCode() throws Exception {
		return "or";
	}

	/**
	 * Private wrapper class for the body of or
	 * 
	 * @author Mgr. Radomir Skrabal
	 * 
	 */
	private static class OrWrapper extends Expression {
		/**
		 * The body expression object
		 */
		public static final OrWrapper singleton = new OrWrapper();

		private OrWrapper() {}

		@Override
		public Expression interpret(Environment env) throws Exception {
			LitBoolean x = (LitBoolean) (env.getVariableValue(new Variable("_x")).interpret(env));
			LitBoolean y = (LitBoolean) (env.getVariableValue(new Variable("_y")).interpret(env));

			if (x == null || y == null) {
				return this;
			}

			return x.value || y.value ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Type infer(Environment env) throws Exception {
			this.setType(TypeConcrete.TypeBool);
			return TypeConcrete.TypeBool;
		}

		@Override
		public Expression substituteTopLevelVariables(Environment topLevel) {
			return this;
		}

		@Override
		public String toClojureCode() throws Exception {
			throw new Exception("Use of OrWrapper for Clojure compilation is not supported.");
		}
	}
}
