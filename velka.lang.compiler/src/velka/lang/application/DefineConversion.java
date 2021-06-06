package velka.lang.application;

import java.util.Arrays;
import java.util.Optional;

import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
import velka.lang.interpretation.ClojureCodeGenerator;
import velka.lang.interpretation.Environment;
import velka.lang.abstraction.Lambda;
import velka.lang.semantic.SemanticParserStatic;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.types.Substitution;
import velka.lang.types.SubstitutionsCannotBeMergedException;
import velka.lang.types.Type;
import velka.lang.types.TypeArrow;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeTuple;
import velka.lang.types.TypesDoesNotUnifyException;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;

/**
 * Expression for defconversion special form
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class DefineConversion extends Expression {

	/**
	 * Type from which we are converting
	 */
	public final TypeAtom from;
	/**
	 * Type to which expression is converting
	 */
	public final TypeAtom to;
	/**
	 * Arguments of the conversion
	 */
	public final Tuple args;
	/**
	 * Body of the conversion
	 */
	public final Expression body;
	

	public DefineConversion(TypeAtom fromType, TypeAtom toType, Tuple args, Expression body) {
		super();
		this.from = fromType;
		this.to = toType;
		this.args = args;
		this.body = body;
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Lambda lambda = this.makeConversionLambda(env);
		
		typeEnv.addConversion(this.from, this.to, lambda);
		return Expression.EMPTY_EXPRESSION;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> p = this.makeConversionLambda(env).infer(env, typeEnv);
		TypeArrow type = (TypeArrow) p.first;
		TypeTuple ttuple = new TypeTuple(Arrays.asList(this.from));
		Optional<Substitution> left = Type.unifyTypes(type.ltype, ttuple);
		if(left.isEmpty()) {
			throw new TypesDoesNotUnifyException(type.ltype, ttuple);
		}
		
		Optional<Substitution> right = Type.unifyTypes(type.rtype, this.to);
		if(right.isEmpty()) {
			throw new TypesDoesNotUnifyException(type.rtype, this.to);
		}
		
		Optional<Substitution> opt = left.get().union(right.get());
		if(opt.isEmpty()) {
			throw new SubstitutionsCannotBeMergedException(left.get(), right.get());
		}
		
		Optional<Substitution> tmp =  opt.get().union(p.second);
		if(tmp.isEmpty()) {
			throw new SubstitutionsCannotBeMergedException(opt.get(), p.second);
		}
		
		return new Pair<Type, Substitution>(Expression.EMPTY_EXPRESSION.infer(env, typeEnv).first, Substitution.EMPTY);
	}

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Lambda conversionLambda = this.makeConversionLambda(env);
		typeEnv.addConversion(this.from, this.to, conversionLambda);
		StringBuilder s = new StringBuilder();
		s.append("(def ^:dynamic ");
		s.append(ClojureCodeGenerator.atomicConversionMapClojureSymbol);
		s.append(" (assoc ");
		s.append(ClojureCodeGenerator.atomicConversionMapClojureSymbol);
		s.append(" [");
		s.append(this.from.clojureTypeRepresentation());
		s.append(" ");
		s.append(this.to.clojureTypeRepresentation());
		s.append("] ");
		s.append(conversionLambda.toClojureCode(env, typeEnv));
		s.append("))");
		
		return s.toString();
	}

	@Override
	public String toString() {
		return "(" + SemanticParserStatic.DEFINE_CONVERSION + " " + from.toString() + " " + to.toString() + " "
				+ this.args.toString() + " " + this.body.toString() + ")";
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof DefineConversion) {
			return this.from.equals(((DefineConversion) other).from) && this.to.equals(((DefineConversion) other).to)
					&& this.args.equals(((DefineConversion) other).args) && this.body.equals(((DefineConversion) other).body);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.from.hashCode() * this.to.hashCode() * this.args.hashCode() * this.body.hashCode();
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof DefineConversion) {
			int cmp = this.from.compareTo(((DefineConversion) other).from);
			if (cmp != 0)
				return cmp;
			cmp = this.to.compareTo(((DefineConversion) other).to);
			if (cmp != 0)
				return cmp;
			cmp = this.args.compareTo(((DefineConversion) other).args);
			if (cmp != 0)
				return cmp;
			return this.body.compareTo(((DefineConversion) other).body);
		}
		return super.compareTo(other);
	}
	
	/**
	 * Creates conversion lambda for this conversion definiton
	 * @param env environment where lambda is created
	 * @return lambda expression
	 */
	private Lambda makeConversionLambda(Environment env) {
		return new Lambda(this.args, new TypeTuple(Arrays.asList(this.from)), this.body);
	}

	/**
	 * Returns true if given lambda is conversion between given from and to types.
	 * Otherwise returns false.
	 * 
	 * @param lambda tested lambda
	 * @param from   type atom
	 * @param to     type atom
	 * @param env    environment
	 * @return true or false
	 * @throws AppendableException if there is issue during inference of lambda
	 */
	public static boolean isLambdaValidConversion(Lambda lambda, TypeAtom from, TypeAtom to, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		Pair<Type, Substitution> infered = lambda.infer(env, typeEnv);
		TypeArrow expected = new TypeArrow(new TypeTuple(Arrays.asList(from)), to);
		return infered.first.equals(expected);
	}
}
