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
 * Wrapper class for Not lambda
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class Not extends Function {
	/**
	 * Wrapped addition lambda object
	 */
	public static final Not singleton = new Not();

	private Not() {
		super(	new TypeTuple(new Type[] {TypeConcrete.TypeBool}),
				new Tuple(new Variable[] { new Variable("_x") }),
				NotWrapper.singleton,
				new Environment());
		this.infer(new Environment());
	}

	@Override
	public String toString() {
		return "NOT";
	}
	
	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		return this;
	}
	
	@Override
	public Type infer(Environment env){
		Type t = new TypeArrow(new TypeTuple(new Type[]{TypeConcrete.TypeBool}), TypeConcrete.TypeBool);
		this.setType(t);
		return t;
	}
	
	@Override
	public String toClojureCode() throws Exception {
		return "not";
	}

	/**
	 * Private wrapper class for the body of not
	 * 
	 * @author Mgr. Radomir Skrabal
	 * 
	 */
	private static class NotWrapper extends Expression {
		/**
		 * The body expression object
		 */
		public static final NotWrapper singleton = new NotWrapper();

		private NotWrapper() {}

		@Override
		public Expression interpret(Environment env) throws Exception {
			LitBoolean x = (LitBoolean) (env.getVariableValue(new Variable("_x")).interpret(env));

			if (x == null) {
				return this;
			}

			return !x.value ? LitBoolean.TRUE : LitBoolean.FALSE;
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
			throw new Exception("Use of NotWrapper for Clojure compilation is not supported.");
		}
	}
}
