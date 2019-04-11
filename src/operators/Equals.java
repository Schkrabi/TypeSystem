package operators;

import interpretation.Environment;
import types.Type;
import types.TypeArrow;
import types.TypeConcrete;
import types.TypeTuple;
import types.TypeVariable;
import util.NameGenerator;

import java.util.Map;
import java.util.TreeMap;

import expression.Expression;
import expression.Function;
import expression.LitBoolean;
import expression.Tuple;
import expression.Variable;

public class Equals extends Function {

	/**
	 * Wrapped equality lambda object
	 */
	public static final Equals singleton = new Equals();

	private Equals() {
		super(new TypeTuple(new Type[] { new TypeVariable("_a"), new TypeVariable("_a") }),
				new Tuple(new Variable[] { new Variable("_x"), new Variable("_y") }), EqWrapper.singleton,
				new Environment());
		this.infer(new Environment());
	}

	@Override
	public String toString() {
		return "equals?";
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		return this;
	}

	@Override
	public Map<Expression, Type> infer(Environment env) {
		Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
		if (this.typeHypothesis == null) {
			Type t = new TypeArrow(new TypeTuple(
					new Type[] { new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()) }),
					TypeConcrete.TypeBool);

			this.typeHypothesis = new TreeMap<Expression, Type>();
			this.typeHypothesis.put(this, t.quantifyUnconstrainedVariables());
		}
		hyp.putAll(this.typeHypothesis);
		return hyp;
	}

	@Override
	public String toClojureCode() throws Exception {
		return "=";
	}

	/**
	 * Private wrapper class for the body of equality
	 * 
	 * @author Mgr. Radomir Skrabal
	 * 
	 */
	private static class EqWrapper extends Expression {
		/**
		 * The body expression object
		 */
		public static final EqWrapper singleton = new EqWrapper();

		private EqWrapper() {
		}

		@Override
		public Expression interpret(Environment env) throws Exception {
			Expression x = (env.getVariableValue(new Variable("_x")).interpret(env));
			Expression y = (env.getVariableValue(new Variable("_y")).interpret(env));

			if (x == null || y == null) {
				return this;
			}

			return x.equals(y) ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Map<Expression, Type> infer(Environment env) {
			Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
			if (this.typeHypothesis == null) {
				Type t = TypeConcrete.TypeBool;
				this.typeHypothesis = new TreeMap<Expression, Type>();
				this.typeHypothesis.put(this, t.quantifyUnconstrainedVariables());
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
			throw new Exception("Use of" + this.getClass().getName() + "for Clojure compilation is not supported.");
		}
	}
}
