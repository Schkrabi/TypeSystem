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
 * Wrapper for the Car (gets first value from a pair)
 * @author Mgr. Radomir Skrabal
 *
 */
public class Car extends Lambda {
	/**
	 * Wrapped addition lambda object
	 */
	public static final Car singleton = new Car();

	private Car() {
		super(new Tuple(
				new Variable[] { new Variable("_x") }),
				CarWrapper.singleton);
	}

	@Override
	public String toString() {
		return "car";
	}
	
	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		return this;
	}
	
	@Override
	public Type infer() throws Exception{
		this.args.infer();
		
		TypeVariable fst = new TypeVariable("_a");
		TypeVariable snd = new TypeVariable("_b");
		
		Type argsType = new TypeTuple(new Type[]{ fst, snd});
		
		Type t = new TypeArrow(new TypeTuple(new Type[]{ argsType }), fst);
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
	private static class CarWrapper extends Expression {
		/**
		 * The body expression object
		 */
		public static final CarWrapper singleton = new CarWrapper();

		private CarWrapper() {
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

			return x.values[0];
		}

		@Override
		public Type infer() throws Exception {
			throw new Exception("Not implemented");
		}

		@Override
		public Expression substituteTopLevelVariables(Environment topLevel) {
			return this;
		}

		@Override
		public String toClojureCode() throws Exception {
			throw new Exception("Use of CarWrapper for Clojure compilation is not supported.");
		}
	}
}
