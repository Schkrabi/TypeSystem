package operators;

import interpretation.Environment;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;
import util.NameGenerator;
import expression.Expression;
import expression.Function;
import expression.Tuple;
import expression.Variable;

public class Deconstruct extends Function {

	/**
	 * Wrapped equality lambda object
	 */
	public static final Deconstruct singleton = new Deconstruct();

	private Deconstruct() {
		super(new TypeTuple(new Type[]{new TypeVariable("_a")}),
				new Tuple(new Variable[] { new Variable("_x")}),
				DeconstructWrapper.singleton, 
				new Environment());
		this.infer(new Environment());
	}

	@Override
	public String toString() {
		return "deconstruct";
	}
	
	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		return this;
	}
	
	@Override
	public Type infer(Environment env){
		Type t = new TypeArrow(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()));
		this.setType(t);
		return t;
	}
	
	@Override
	public String toClojureCode() throws Exception {
		return "(fn [x] x)";
	}

	/**
	 * Private wrapper class for the body of deconstruct
	 * 
	 * @author Mgr. Radomir Skrabal
	 * 
	 */
	private static class DeconstructWrapper extends Expression {
		/**
		 * The body expression object
		 */
		public static final DeconstructWrapper singleton = new DeconstructWrapper();

		private DeconstructWrapper() {
		}

		@Override
		public Expression interpret(Environment env) throws Exception {
			Expression x = (env.getVariableValue(new Variable("_x")).interpret(env));
			
			if (x == null) {
				return this;
			}
			
			Type t = x.infer(env);
			x.setType(t);

			return x;
		}

		@Override
		public Type infer(Environment env) {
			return new TypeVariable(NameGenerator.next());
		}

		@Override
		public Expression substituteTopLevelVariables(Environment topLevel) {
			return this;
		}

		@Override
		public String toClojureCode() throws Exception {
			throw new Exception("Use of" + this.getClass().getName() + "for Clojure compilation is not supported.");
		}
	}

}
