package operators;

import interpretation.Environment;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import expression.Expression;
import expression.Function;
import expression.Tuple;
import expression.Variable;

/**
 * Wrapper for the Cdr (gets the second value of pair
 * @author Mgr. Radomir Skrabal
 *
 */
public class Cdr extends Function {
	/**
	 * Wrapped addition lambda object
	 */
	public static final Cdr singleton = new Cdr();

	private Cdr() {
		super(new TypeTuple(new Type[] {new TypeTuple(new Type[] {new TypeVariable("_a"), new TypeVariable("_b")})}),
				new Tuple(new Expression[] {new Variable("_x")}),
				CdrWrapper.singleton,
				new Environment());
		this.infer(new Environment());
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
		TypeVariable fst = (TypeVariable)((TypeTuple)this.argsType.values[0]).values[0];
		TypeVariable snd = (TypeVariable)((TypeTuple)this.argsType.values[0]).values[1];
		
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
		public Type infer(Environment env) throws AppendableException {
			throw new AppendableException("Not implemented");
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
