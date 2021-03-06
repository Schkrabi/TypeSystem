package velka.core.expression;

import velka.util.AppendableException;
import velka.util.Pair;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeTuple;

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
	public abstract Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException;

	/**
	 * Infers type of expression and returns used substitutions
	 * 
	 * @return Pair of infered type and used substitution
	 * @throws AppendableException
	 */
	public abstract Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException;

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
	public abstract String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException;

	/**
	 * Empty expression
	 */
	public static final Expression EMPTY_EXPRESSION = new Expression() {
		@Override
		public Expression interpret(Environment env, TypeEnvironment typeEnv) {
			return this;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			return new Pair<Type, Substitution>(TypeTuple.EMPTY_TUPLE, Substitution.EMPTY);
		}

		@Override
		public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "(with-meta [] {:lang-type " + TypeTuple.EMPTY_TUPLE.clojureTypeRepresentation() + "})";
		}
		
		@Override
		public String toString() {
			return "[]";
		}
	};
}
