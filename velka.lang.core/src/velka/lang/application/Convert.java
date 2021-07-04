package velka.lang.application;

import velka.lang.expression.Expression;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;

import java.util.Optional;

import velka.lang.conversions.Conversions;
import velka.lang.coreExceptions.ConversionException;
import velka.lang.types.Substitution;
import velka.lang.types.SubstitutionsCannotBeMergedException;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypesDoesNotUnifyException;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;

/**
 * Special form for explicit user invoced conversion of representation
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Convert extends Expression {
	
	/**
	 * Symbol for convert special form
	 */
	public static final String CONVERT = "convert";

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
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		return Conversions.convert(from, this.expression, this.to, typeEnv).interpret(env, typeEnv);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> p = this.expression.infer(env, typeEnv);
		Optional<Substitution> s = Type.unifyTypes(this.from, p.first);
		if(s.isEmpty()) {
			throw new TypesDoesNotUnifyException(this.from, p.first);
		}
		
		if (!typeEnv.canConvert(this.from, this.to)) {
			throw new ConversionException(this.from, this.to, this.expression);
		}

		Optional<Substitution> opt = s.get().union(p.second);
		if(opt.isEmpty()) {
			throw new SubstitutionsCannotBeMergedException(s.get(), p.second);
		}
		
		return new Pair<Type, Substitution>(this.to, opt.get());
	}

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		return Conversions.convert(this.from, this.expression, to, typeEnv).toClojureCode(env, typeEnv);
	}

	@Override
	public String toString() {
		return "(" + CONVERT + " " + this.from.toString() + " " + this.to.toString() + " "
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
