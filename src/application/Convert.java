package application;

import expression.Expression;
import interpretation.Environment;
import semantic.SemanticParserStatic;
import semantic.TypeEnvironment;
import types.ConversionException;
import types.Substitution;
import types.Type;
import types.TypeAtom;
import util.AppendableException;
import util.Pair;

/**
 * Special form for explicit user invoced conversion of representation
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Convert extends Expression {

	/**
	 * Type
	 */
	public final TypeAtom from;
	/**
	 * Type
	 */
	public final TypeAtom to;
	/**
	 * Converted expression
	 */
	public final Expression expression;

	public Convert(TypeAtom from, TypeAtom to, Expression expression) {
		this.from = from;
		this.to = to;
		this.expression = expression;
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		return from.convertTo(this.expression, this.to).interpret(env);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		Pair<Type, Substitution> p = this.expression.infer(env);
		Substitution s = Type.unifyTypes(this.from, p.first);
		if (!TypeEnvironment.singleton.canConvert(this.from, this.to)) {
			throw new ConversionException(this.from, this.to, this.expression);
		}

		s.union(p.second);
		return new Pair<Type, Substitution>(this.to, s);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		return this.from.convertTo(this.expression, to).toClojureCode(env);
	}

	@Override
	public String toString() {
		return "(" + SemanticParserStatic.CONVERT + " " + this.from.toString() + " " + this.to.toString() + " "
				+ this.expression.toString() + ")";
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Convert) {
			return this.from.equals(((Convert) other).from) && this.to.equals(((Convert) other).to)
					&& this.expression.equals(((Convert) other).expression);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.from.hashCode() * this.to.hashCode() * this.expression.hashCode();
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof Convert) {
			int cmp = this.from.compareTo(((Convert) other).from);
			if (cmp != 0)
				return cmp;
			cmp = this.to.compareTo(((Convert) other).to);
			if (cmp != 0)
				return cmp;
			return this.expression.compareTo(((Convert) other).expression);
		}
		return super.compareTo(other);
	}
}
