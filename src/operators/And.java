package operators;

import interpretation.Environment;
import types.Type;
import types.TypeArrow;
import types.TypeConcrete;
import types.TypeTuple;

import java.util.Map;
import java.util.TreeMap;

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
public class And extends Function {
	/**
	 * Wrapped addition lambda object
	 */
	public static final And singleton = new And();

	private And() {
		super(new TypeTuple(new Type[] { TypeConcrete.TypeBool, TypeConcrete.TypeBool }),
				new Tuple(new Variable[] { new Variable("_x"), new Variable("_y") }), AndWrapper.singleton,
				new Environment());
		this.infer(new Environment());
	}

	@Override
	public String toString() {
		return "AND";
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
					new TypeTuple(new Type[] { TypeConcrete.TypeBool, TypeConcrete.TypeBool }), TypeConcrete.TypeBool));
		}
		hyp.putAll(this.typeHypothesis);
		return hyp;
	}

	@Override
	public String toClojureCode() throws Exception {
		return "and";
	}

	/**
	 * Private wrapper class for the body of and
	 * 
	 * @author Mgr. Radomir Skrabal
	 * 
	 */
	private static class AndWrapper extends Expression {
		/**
		 * The body expression object
		 */
		public static final AndWrapper singleton = new AndWrapper();

		private AndWrapper() {
		}

		@Override
		public Expression interpret(Environment env) throws Exception {
			LitBoolean x = (LitBoolean) (env.getVariableValue(new Variable("_x")).interpret(env));
			LitBoolean y = (LitBoolean) (env.getVariableValue(new Variable("_y")).interpret(env));

			if (x == null || y == null) {
				return this;
			}

			return x.value && y.value ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Map<Expression, Type> infer(Environment env) {
			Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
			if (this.typeHypothesis == null) {
				Type t = TypeConcrete.TypeBool;
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
			throw new Exception("Use of AndWrapper for Clojure compilation is not supported.");
		}
	}
}
