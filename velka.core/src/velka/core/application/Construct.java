package velka.core.application;

import java.util.Iterator;

import velka.core.abstraction.Abstraction;
import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.util.AppendableException;
import velka.util.Pair;

/**
 * Special form for constructing types
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Construct extends Expression {
	
	/**
	 * Symbol for construct special form
	 */
	public static final String CONSTRUCT = "construct";

	/**
	 * Constructed type atom
	 */
	public final TypeAtom constructedType;

	/**
	 * Arguments supplied to the constructor
	 */
	public final Tuple arguments;

	public Construct(TypeAtom constructedType, Tuple arguments) {
		this.constructedType = constructedType;
		this.arguments = arguments;
	}

	/**
	 * Selects correct constructor and creates application for it with given
	 * arguments
	 * 
	 * @param env environment where construction is evaluated
	 * @return Application
	 * @throws AppendableException if error during inference or no suitable
	 *                             constructor found
	 */
	private Application deriveApplication(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		TypeTuple argumentsType = (TypeTuple) this.arguments.infer(env, typeEnv).first;
		Abstraction constructor = typeEnv.getConstructor(this.constructedType, argumentsType);
		return new AbstractionApplication(constructor, this.arguments);
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Application application = this.deriveApplication(env, typeEnv);
		return application.interpret(env, typeEnv);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> argumentsInfered = this.arguments.infer(env, typeEnv);
		return new Pair<Type, Substitution>(this.constructedType, argumentsInfered.second);
	}

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Application application = this.deriveApplication(env, typeEnv);
		return application.toClojureCode(env, typeEnv);
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder();
		s.append("(");
		s.append(CONSTRUCT);
		s.append(" ");
		s.append(this.constructedType.name.toString());
		s.append(" ");
		s.append(this.constructedType.representation.toString());
		if (!this.arguments.equals(Tuple.EMPTY_TUPLE)) {
			s.append(" ");

			Iterator<Expression> i = this.arguments.iterator();
			while (i.hasNext()) {
				Expression e = i.next();
				s.append(e.toString());
				if (i.hasNext()) {
					s.append(" ");
				}
			}
		}
		s.append(")");
		return s.toString();
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Construct) {
			return this.constructedType.equals(((Construct) other).constructedType)
					&& this.arguments.equals(((Construct) other).arguments);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.constructedType.hashCode() * this.arguments.hashCode();
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof Construct) {
			int cmp = this.constructedType.compareTo(((Construct) other).constructedType);
			if (cmp != 0)
				return cmp;
			return this.arguments.compareTo(((Construct) other).arguments);
		}
		return super.compareTo(other);
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		Expression e = this.interpret(env, typeEnv);
		return e.convert(to, env, typeEnv);
	}
}
