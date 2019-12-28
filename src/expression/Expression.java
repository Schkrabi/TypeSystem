package expression;

import types.Substitution;
import types.Type;
import types.TypeArrow;
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
	 * @param env
	 *            environment where the expression should be interpreted
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
	 * @return string containing clojure expression
	 * @throws AppendableException
	 */
	public abstract String toClojureCode() throws AppendableException;
	
	/**
	 * Transforms expression into equivalent clojure expression
	 * @param expectedType Expected type of expression
	 * @return string containing clojure expression
	 * @throws AppendableException
	 */
	protected abstract String toClojureCode(Type expectedType, Environment env) throws AppendableException;
	
	/**
	 * Gets function type with specific representations in place
	 * @param env Environment in which inference is carried out
	 * @return TypeArrow
	 * @throws AppendableException When used with invalid expression
	 */
	public TypeArrow getFunctionTypeWithRepresentations(TypeTuple argTypes, Environment env) throws AppendableException {
		throw new AppendableException(this.getClass().getName() + " cannot use " + new Throwable().getStackTrace()[0].getMethodName());
	}

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
		public String toClojureCode() {
			return this.toClojureCode(null, Environment.topLevelEnvironment);
		}

		@Override
		public String toClojureCode(Type expectedType, Environment env) {
			return "nil";
		}
	};
}
