package expression;

import java.util.Arrays;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;

/**
 * Expression for interpretation and handling the define special form. Purely
 * for purposes of simplifying the code.
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class DefExpression extends Expression {

	/**
	 * Name of defined expression
	 */
	public final Variable name;
	/**
	 * Defined expression
	 */
	public final Expression defined;

	public DefExpression(Variable name, Expression defined) {
		this.name = name;
		this.defined = defined;
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		//TODO!!!
		Environment e = Environment.create(env);
		e.put(this.name, new TypeHolder(new TypeVariable(NameGenerator.next()), this.name));
		Expression interpreted = this.defined.interpret(e);
		env.put(this.name, interpreted);
		return Expression.EMPTY_EXPRESSION;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		try {
			Environment childEnv = Environment.create(env);

			// Define creates new binding in environment, need to have reference to type of
			// this variable existing in environment from we are infering
			TypeVariable tv = new TypeVariable(NameGenerator.next());
			childEnv.put(this.name, new TypeHolder(tv));
			Pair<Type, Substitution> infered = this.defined.infer(childEnv);

			Substitution s = infered.second;
			Substitution tmp;

			if (!s.containsVariable(tv)) {
				tmp = new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(tv, infered.first)));
			} else {
				tmp = Type.unify(s.get(tv).get(), infered.first);
			}
			s = s.union(tmp);

			return new Pair<Type, Substitution>(TypeTuple.EMPTY_TUPLE, s);

		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public String toClojureCode() throws AppendableException {
		return this.toClojureCode(TypeTuple.EMPTY_TUPLE, Environment.topLevelEnvironment);
	}
	
	@Override
	protected String toClojureCode(Type expectedType, Environment env) throws AppendableException {
		StringBuilder s = new StringBuilder("(def ");
		s.append(this.name.toClojureCode(new TypeVariable(NameGenerator.next()), env));
		s.append(" ");
		Type t = this.defined.infer(env).first;
		env.put(this.name, new TypeHolder(t));
		s.append(this.defined.toClojureCode(t, env));
		s.append(")");
		return s.toString();
	}

	@Override
	public String toString() {
		return "(define " + this.name.toString() + " " + this.defined.toString() + ")";
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof DefExpression) {
			DefExpression o = (DefExpression) other;
			int c = this.name.compareTo(o.name);
			if (c != 0)
				return c;
			return this.defined.compareTo(o.defined);
		}
		return super.compareTo(other);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof DefExpression) {
			return this.name.equals(((DefExpression) other).name)
					&& this.defined.equals(((DefExpression) other).defined);
		}
		return false;
	}
	
	@Override
	public int hashCode() {
		return this.defined.hashCode() * this.name.hashCode();
	}
}
