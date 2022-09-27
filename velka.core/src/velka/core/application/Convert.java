package velka.core.application;

import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypesDoesNotUnifyException;

import java.util.Optional;

import velka.core.exceptions.ConversionException;
import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.Pair;

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
	public final Type from;
	/**
	 * Type
	 */
	public final Type to;
	/**
	 * Converted expression
	 */
	public final Expression expression;

	public Convert(Type from, Type to, Expression expression) {
		this.from = from;
		this.to = to;
		this.expression = expression;
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		return this.expression.convert(this.to, env, typeEnv);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> p = this.expression.infer(env, typeEnv);
		Optional<Substitution> s = Type.unifyTypes(this.from, p.first);
		if(s.isEmpty()) {
			throw new TypesDoesNotUnifyException(this.from, p.first);
		}
		
		if (!typeEnv.canConvert(this.from, this.to)) {
			throw new ConversionException(this.to, this.expression);
		}
		
		return new Pair<Type, Substitution>(this.to, s.get().compose(p.second));
	}

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		return ClojureHelper.applyClojureFunction(ClojureCoreSymbols.convertClojureSymbol_full,
				this.to.clojureTypeRepresentation(), this.expression.toClojureCode(env, typeEnv));
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

	@Override
	protected Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		throw new ConversionException(to, this);
	}
}
