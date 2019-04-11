package operators;

import java.util.Map;
import java.util.TreeMap;

import expression.Expression;
import expression.Function;
import expression.LitInteger;
import expression.Tuple;
import expression.Variable;
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
public class Addition extends Function {

	/**
	 * Wrapped addition lambda object
	 */
	public static final Addition singleton = new Addition();

	private Addition() {
		super(new TypeTuple(new Type[] { TypeConcrete.TypeInt, TypeConcrete.TypeInt }),
				new Tuple(new Variable[] { new Variable("_x"), new Variable("_y") }), AddWrapper.singleton,
				new Environment());
		this.infer(new Environment());
	}

	@Override
	public String toString() {
		return "+";
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		return this;
	}

	@Override
	public Map<Expression, Type> infer(Environment env) {
		Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
		if (this.typeHypothesis == null) {
			this.typeHypothesis = new TreeMap<Expression, Type>();
			this.typeHypothesis.put(this, new TypeArrow(
					new TypeTuple(new Type[] { TypeConcrete.TypeInt, TypeConcrete.TypeInt }), TypeConcrete.TypeInt));
		}
		hyp.putAll(this.typeHypothesis);
		return hyp;
	}

	@Override
	public String toClojureCode() throws Exception {
		return "+";
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
			LitInteger x = (LitInteger) (env.getVariableValue(new Variable("_x")).interpret(env));
			LitInteger y = (LitInteger) (env.getVariableValue(new Variable("_y")).interpret(env));

			if (x == null || y == null) {
				return this;
			}

			return new LitInteger(x.value + y.value);
		}

		@Override
		public Map<Expression, Type> infer(Environment env) {
			Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
			if (this.typeHypothesis == null) {
				Type t = TypeConcrete.TypeInt;
				this.typeHypothesis = new TreeMap<Expression, Type>();
				this.typeHypothesis.put(this, t);
			}
			hyp.putAll(this.typeHypothesis);
			return hyp;
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
