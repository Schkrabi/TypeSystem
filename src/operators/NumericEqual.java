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
import expression.LitInteger;
import expression.Tuple;
import expression.Variable;

public class NumericEqual extends Function {

	/**
	 * Wrapped equality lambda object
	 */
	public static final NumericEqual singleton = new NumericEqual();

	private NumericEqual() {
		super(new TypeTuple(new Type[]{TypeConcrete.TypeInt, TypeConcrete.TypeInt}),
				new Tuple(new Variable[] { new Variable("_x"), new Variable("_y") }),
				EqWrapper.singleton, 
				new Environment());
		this.infer(new Environment());
	}

	@Override
	public String toString() {
		return "=";
	}
	
	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		return this;
	}
	
	@Override
	public Map<Expression, Type> infer(Environment env){
		Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
		if(this.typeHypothesis == null) {
			Type t = new TypeArrow(new TypeTuple(new Type[]{TypeConcrete.TypeInt, TypeConcrete.TypeInt}), TypeConcrete.TypeBool);
			
			this.typeHypothesis = new TreeMap<Expression, Type>();
			this.typeHypothesis.put(this, t.quantifyUnconstrainedVariables());
		}
		hyp.putAll(this.typeHypothesis);
		return hyp;
	}
	
	@Override
	public String toClojureCode() throws Exception {
		return "==";
	}

	/**
	 * Private wrapper class for the body of addition
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
			LitInteger x = (LitInteger) (env.getVariableValue(new Variable("_x")).interpret(env));
			LitInteger y = (LitInteger) (env.getVariableValue(new Variable("_y")).interpret(env));

			if (x == null || y == null) {
				return this;
			}

			return x.value == y.value ? LitBoolean.TRUE : LitBoolean.FALSE;
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
