package operators;

import interpretation.Environment;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;

import java.util.Map;
import java.util.TreeMap;

import expression.Expression;
import expression.Function;
import expression.Tuple;
import expression.Variable;

@Deprecated
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
	public Map<Expression, Type> infer(Environment env){
		Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
		if(this.typeHypothesis == null) {
			Type t = new TypeArrow(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()));
			
			this.typeHypothesis = new TreeMap<Expression, Type>();
			this.typeHypothesis.put(this, t.quantifyUnconstrainedVariables());
		}
		hyp.putAll(this.typeHypothesis);
		return hyp;
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
		public Expression interpret(Environment env) throws AppendableException {
			//TODO not implemented
			throw new AppendableException("Not implemented");
		}

		@Override
		public Map<Expression, Type> infer(Environment env) throws AppendableException {
			//TODO
			throw new AppendableException("Not implemented");
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
