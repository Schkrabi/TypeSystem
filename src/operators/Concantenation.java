package operators;

import interpretation.Environment;
import types.Type;
import types.TypeArrow;
import types.TypeConcrete;
import types.TypeTuple;
import expression.Expression;
import expression.Lambda;
import expression.LitString;
import expression.Tuple;
import expression.Variable;

/**
 * Wrapper class for string concatenation lambda
 * @author Mgr. Radomir Skrabal
 *
 */
public class Concantenation extends Lambda {
	/**
	 * Wrapped addition lambda object
	 */
	public static final Concantenation singleton = new Concantenation();

	private Concantenation() {
		super(new Tuple(
				new Variable[] { new Variable("_x"), new Variable("_y") }),
				ConCatWrapper.singleton);
		this.infer(new Environment());
	}

	@Override
	public String toString() {
		return "concat";
	}
	
	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		return this;
	}
	
	@Override
	public Type infer(Environment env){
		Type t = new TypeArrow(new TypeTuple(new Type[]{TypeConcrete.TypeString, TypeConcrete.TypeString}), TypeConcrete.TypeString);
		this.setType(t);
		return t;
	}
	
	@Override
	public String toClojureCode() throws Exception {
		return "concat";
	}

	/**
	 * Private wrapper class for the body of concantenation
	 * 
	 * @author Mgr. Radomir Skrabal
	 * 
	 */
	private static class ConCatWrapper extends Expression {
		/**
		 * The body expression object
		 */
		public static final ConCatWrapper singleton = new ConCatWrapper();

		private ConCatWrapper() {
		}

		@Override
		public Expression interpret(Environment env) throws Exception {
			LitString x = (LitString) (env.getVariableValue(new Variable("_x")).interpret(env));
			LitString y = (LitString) (env.getVariableValue(new Variable("_y")).interpret(env));

			if (x == null || y == null) {
				return this;
			}

			return new LitString(x.value + y.value);
		}

		@Override
		public Type infer(Environment env) throws Exception {
			this.setType(TypeConcrete.TypeString);
			return TypeConcrete.TypeString;
		}

		@Override
		public Expression substituteTopLevelVariables(Environment topLevel) {
			return this;
		}

		@Override
		public String toClojureCode() throws Exception {
			throw new Exception("Use of ConCatWrapper for Clojure compilation is not supported.");
		}
	}
}
