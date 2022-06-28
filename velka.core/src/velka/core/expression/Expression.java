package velka.core.expression;

import velka.util.AppendableException;
import velka.util.Pair;
import velka.core.exceptions.ConversionException;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.RepresentationOr;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeTuple;
import velka.types.TypeVariable;

/**
 * Abstract superclass for all expressions
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public abstract class Expression implements Comparable<Expression>, IConvertable<Expression> {
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
	 * Does the conversion logic
	 * @param to type to which expression is converted
	 * @param env environment where conversion takes place
	 * @param typeEnv type environment where conversion takes place
	 * @return Expression
	 * @throws AppendableException if conversion is invalid
	 */
	protected abstract Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv) throws AppendableException;
	
	@Override
	public Expression convert(Type to, Environment env, TypeEnvironment typeEnv) throws AppendableException {
		if(to instanceof TypeVariable) {
			return this;
		}
		Pair<Type, Substitution> p = this.infer(env, typeEnv);
		
		if(Type.unifyRepresentation(p.first, to).isPresent()) {
			return this;
		}
		if(!typeEnv.canConvert(p.first, to)) {
			throw new ConversionException(to, this);
		}
		if(to instanceof RepresentationOr) {
			for(Type t : ((RepresentationOr)to).getRepresentations()) {
				if(Type.unifyRepresentation(p.first, t).isPresent()) {
					return this;
				}
			}
			throw new ConversionException(to, this);
		}
		
		Expression e = this.doConvert(p.first, to, env, typeEnv);
		
		Pair<Type, Substitution> q = e.infer(env, typeEnv);
		if(p.first.equals(q.first)) {
			return this;
		}
		return e;
	}

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
			return Type.addTypeMetaInfo("[]", TypeTuple.EMPTY_TUPLE);
		}
		
		@Override
		public String toString() {
			return "[]";
		}

		@Override
		protected Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			if(!to.equals(TypeTuple.EMPTY_TUPLE)) {
				throw new ConversionException(to, this);
			}
			return this;
		}
	};
}
