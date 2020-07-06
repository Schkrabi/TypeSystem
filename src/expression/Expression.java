package expression;

import types.Substitution;
import types.Type;
import types.TypeTuple;
import util.AppendableException;
import util.Pair;

import interpretation.Environment;

/**
 * Abstract superclass for all expressions
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public abstract class Expression implements Comparable<Expression> {
	/**
	 * Interprets the expression in given environment
	 * 
	 * @param env environment where the expression should be interpreted
	 * @return Expression
	 * @throws Exception
	 */
	public abstract Expression interpret(Environment env) throws AppendableException;

	/**
	 * Infers type of expression and returns used substitutions
	 * 
	 * @return Pair of infered type and used substitution
	 * @throws AppendableException
	 */
	public abstract Pair<Type, Substitution> infer(Environment env) throws AppendableException;

	@Override
	public int compareTo(Expression other) {
		return this.getClass().getName().compareTo(other.getClass().getName());
	}

	/**
	 * Transforms expression into equivalent clojure expression
	 * 
	 * @return string containing clojure expression
	 * @throws AppendableException
	 */
	public String toClojureCode() throws AppendableException
	{
		return this.toClojureCode(Environment.topLevelEnvironment);
	}

	/**
	 * Transforms expression into equivalent clojure expression
	 * 
	 * @return string containing clojure expression
	 * @throws AppendableException
	 */
	public abstract String toClojureCode(Environment env) throws AppendableException;

	/**
	 * Empty expression
	 */
	public static final Expression EMPTY_EXPRESSION = new Expression() {
		@Override
		public Expression interpret(Environment env) {
			return this;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			return new Pair<Type, Substitution>(TypeTuple.EMPTY_TUPLE, Substitution.EMPTY);
		}

		@Override
		public String toClojureCode(Environment env) {
			return "nil";
		}
		
		@Override
		public String toString() {
			return "nil";
		}
	};
}
