package velka.lang.application;

import java.util.Iterator;

import velka.lang.abstraction.Abstraction;
import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
import velka.lang.interpretation.Environment;
import velka.lang.semantic.SemanticParserStatic;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeTuple;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;

/**
 * Special form for constructing types
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Construct extends Expression {

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
	private Application deriveApplication(Environment env) throws AppendableException {
		TypeTuple argumentsType = (TypeTuple) this.arguments.infer(env).first;
		Abstraction constructor = TypeEnvironment.singleton.getConstructor(this.constructedType, argumentsType);
		return new AbstractionApplication(constructor, this.arguments);
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		Application application = this.deriveApplication(env);
		return application.interpret(env);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		Pair<Type, Substitution> argumentsInfered = this.arguments.infer(env);
		return new Pair<Type, Substitution>(this.constructedType, argumentsInfered.second);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		Application application = this.deriveApplication(env);
		return application.toClojureCode(env);
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder();
		s.append("(");
		s.append(SemanticParserStatic.CONSTRUCT);
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
}
