package expression;

import types.Type;
import types.TypeTuple;
import util.AppendableException;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import expression.Literal.ConversionWrapper;
import interpretation.Environment;

/**
 * Abstract superclass for all expressions
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public abstract class Expression implements Comparable<Expression> {
	/**
	 * Type inferred by the infer method when it is run
	 */
	protected Map<Expression, Type> typeHypothesis = null;

	/**
	 * Interprets the expression in given environment
	 * 
	 * @param env
	 *            environment where the expression should be interpreted
	 * @return Expression
	 * @throws Exception
	 */
	public abstract Expression interpret(Environment env) throws Exception;
	
	/**
	 * Gets infered types of this expression
	 * @param env
	 * @return
	 * @throws AppendableException
	 */
	public Map<Expression, Type> getTypeHypothesis(Environment env) throws AppendableException{
		Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
		if(this.typeHypothesis == null) {
			this.typeHypothesis = this.infer(env);
		}
		hyp.putAll(this.typeHypothesis);
		return hyp;
	}

	/**
	 * Infers the type of the expression and all its subexpression and returns it.
	 * Also sets the inferedType variable of each inferred expression
	 * 
	 * @return inferred type
	 * @throws Exception
	 */
	public abstract Map<Expression, Type> infer(Environment env) throws AppendableException;

	/**
	 * Returns the inferred type of this expression or null if inference was not run
	 * 
	 * @return Type or null
	 */
	public Type getType() {
		if (this.typeHypothesis == null) {
			return null;
		}
		return this.typeHypothesis.get(this);
	}

	@Override
	public int compareTo(Expression other) {
		return (int) Math.signum(ordering.indexOf(this.getClass()) - ordering.indexOf(other.getClass()));
	}

	public abstract Expression substituteTopLevelVariables(Environment topLevel) throws Exception;

	public abstract String toClojureCode() throws Exception;

	private static List<Class<? extends Expression>> ordering = Arrays.asList(Application.class,
			DefExpression.class, ExceptionExpr.class, IfExpression.class, Literal.class, MetaFunction.class,
			MetaLambda.class, QuotedExpression.class, Sequence.class, Tuple.class, Variable.class,
			ConversionWrapper.class, EmptyExpression.class);
	
	/**
	 * Empty expression
	 */
	public static final Expression EMPTY_EXPRESSION = new EmptyExpression();

	private static class EmptyExpression extends Expression {

		@Override
		public Expression interpret(Environment env) throws Exception {
			return this;
		}

		@Override
		public Map<Expression, Type> infer(Environment env) {
			if (this.typeHypothesis == null) {
				this.typeHypothesis = new TreeMap<Expression, Type>();
				this.typeHypothesis.put(this, TypeTuple.EMPTY_TUPLE);
			}
			Map<Expression, Type> r = new TreeMap<Expression, Type>();
			r.putAll(this.typeHypothesis);
			return r;
		}

		@Override
		public Expression substituteTopLevelVariables(Environment topLevel) {
			return this;
		}

		@Override
		public String toClojureCode() throws Exception {
			return "";
		}

	}
}
