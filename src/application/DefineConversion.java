package application;

import java.util.Arrays;

import expression.Expression;
import interpretation.Environment;
import abstraction.Lambda;
import semantic.SemanticParserStatic;
import semantic.TypeEnvironment;
import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeAtom;
import types.TypeTuple;
import util.AppendableException;
import util.Pair;

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
	TypeAtom from;
	/**
	 * Type to which expression is converting
	 */
	TypeAtom to;
	/**
	 * Conversion lambda
	 */
	Lambda lambda;

	private DefineConversion(TypeAtom fromType, TypeAtom toType, Lambda lambda) {
		super();
		this.from = fromType;
		this.to = toType;
		this.lambda = lambda;
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		TypeEnvironment.singleton.addConversion(this.from, this.to, this.lambda);
		return Expression.EMPTY_EXPRESSION;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		Pair<Type, Substitution> p = this.lambda.infer(env);
		TypeArrow type = (TypeArrow) p.first;
		Substitution s = Type.unifyTypes(type.ltype, new TypeTuple(Arrays.asList(this.from)));
		s = s.union(Type.unifyTypes(type.rtype, this.to));
		s = s.union(p.second);
		return new Pair<Type, Substitution>(Expression.EMPTY_EXPRESSION.infer(env).first, s);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		TypeEnvironment.singleton.addConversion(this.from, this.to, this.lambda);
		return "";
	}

	@Override
	public String toString() {
		return "(" + SemanticParserStatic.DEFINE_CONVERSION + " " + from.toString() + " " + to.toString() + " "
				+ this.lambda + ")";
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof DefineConversion) {
			return this.from.equals(((DefineConversion) other).from) && this.to.equals(((DefineConversion) other).to)
					&& this.lambda.equals(((DefineConversion) other).lambda);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.from.hashCode() * this.to.hashCode() * this.lambda.hashCode();
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
			return this.lambda.compareTo(((DefineConversion) other).lambda);
		}
		return super.compareTo(other);
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
	public static boolean isLambdaValidConversion(Lambda lambda, TypeAtom from, TypeAtom to)
			throws AppendableException {
		Pair<Type, Substitution> infered = lambda.infer(Environment.topLevelEnvironment);
		TypeArrow expected = new TypeArrow(new TypeTuple(Arrays.asList(from)), to);
		return infered.first.equals(expected);
	}

	/**
	 * Creates instance
	 * 
	 * @param from
	 * @param to
	 * @param lambda
	 * @return
	 * @throws AppendableException
	 */
	public static DefineConversion makeDefineConversion(TypeAtom from, TypeAtom to, Lambda lambda)
			throws AppendableException {
		if (!DefineConversion.isLambdaValidConversion(lambda, from, to)) {
			throw new AppendableException(
					"Invalid conversion expected " + new TypeArrow(new TypeTuple(Arrays.asList(from)), to) + " got "
							+ lambda.infer(Environment.topLevelEnvironment).first);
		}
		return new DefineConversion(from, to, lambda);
	}
}
