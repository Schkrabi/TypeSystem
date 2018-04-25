package operators;

import interpretation.Environment;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import expression.Expression;
import expression.Lambda;
import expression.Tuple;
import expression.Variable;

/**
 * Wrapper for the Cdr (gets the second value of pair
 * @author Mgr. Radomir Skrabal
 *
 */
public class Cdr extends Lambda {
	/**
	 * Wrapped addition lambda object
	 */
	public static final Cdr singleton = new Cdr();

	private Cdr() {
		super(new Tuple(
				new Variable[] { new Variable("_x") }),
				CdrWrapper.singleton);
	}

	@Override
	public String toString() {
		return "cdr";
	}
	
	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		return this;
	}
	
	@Override
	public Type infer(Environment env){
		TypeVariable fst = new TypeVariable("_a");
		TypeVariable snd = new TypeVariable("_b");
		
		Type t = new TypeArrow(new TypeTuple(new Type[]{ new TypeTuple(new Type[]{ fst, snd})}), snd);
		this.setType(t);
		return t;
	}
	
	@Override
	public String toClojureCode() throws Exception {
		throw new Exception("Not implemented");
	}

	/**
	 * Private wrapper class for the body of car
	 * 
	 * @author Mgr. Radomir Skrabal
	 * 
	 */
	private static class CdrWrapper extends Expression {
		/**
		 * The body expression object
		 */
		public static final CdrWrapper singleton = new CdrWrapper();

		private CdrWrapper() {
		}

		@Override
		public Expression interpret(Environment env) throws Exception {
			Tuple x = (Tuple) (env.getVariableValue(new Variable("_x")).interpret(env));
			
			if (x == null) {
				return this;
			}
			
			if(x.values.length != 2){
				throw new AppendableException("Argument of car is " + x + " pair expected");
			}

			return x.values[1];
		}

		@Override
		public Type infer(Environment env) throws Exception {
			throw new Exception("Not implemented");
		}

		@Override
		public Expression substituteTopLevelVariables(Environment topLevel) {
			return this;
		}

		@Override
		public String toClojureCode() throws Exception {
			throw new Exception("Use of CdrWrapper for Clojure compilation is not supported.");
		}
	}
}
