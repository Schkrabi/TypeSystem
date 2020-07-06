package application;

import java.util.Arrays;

import expression.Expression;
import expression.TypeHolder;
import expression.Symbol;
import interpretation.Environment;
import semantic.SemanticParserStatic;
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
public class DefineSymbol extends Expression {

	/**
	 * Name of defined expression
	 */
	public final Symbol name;
	/**
	 * Defined expression
	 */
	public final Expression defined;

	public DefineSymbol(Symbol name, Expression defined) {
		this.name = name;
		this.defined = defined;
	}

	/**
	 * Infers type and substitution of defined expression
	 * 
	 * @param env Environment where Define is evaluated
	 * @return Pair of Type and used Substitution
	 * @throws AppendableException
	 */
	public Pair<Type, Substitution> inferDefined(Environment env) throws AppendableException {
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

		return new Pair<Type, Substitution>(infered.first, s);
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		Environment e = Environment.create(env);
		Pair<Type, Substitution> innerInfered = this.inferDefined(env);
		e.put(this.name, new TypeHolder(innerInfered.first, this.name));
		Expression interpreted = this.defined.interpret(e);
		env.put(this.name, interpreted);
		return Expression.EMPTY_EXPRESSION;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		try {
			Pair<Type, Substitution> definedInfered = this.inferDefined(env);

			return new Pair<Type, Substitution>(TypeTuple.EMPTY_TUPLE, definedInfered.second);

		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		StringBuilder s = new StringBuilder("(def ");
		s.append(this.name.toClojureCode(env));
		s.append(" ");
		Type t = this.defined.infer(env).first;
		env.put(this.name, new TypeHolder(t));
		s.append(this.defined.toClojureCode(env));
		s.append(")");
		return s.toString();
	}

	@Override
	public String toString() {
		return "(" + SemanticParserStatic.DEFINE + " " + this.name.toString() + " " + this.defined.toString() + ")";
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof DefineSymbol) {
			DefineSymbol o = (DefineSymbol) other;
			int c = this.name.compareTo(o.name);
			if (c != 0)
				return c;
			return this.defined.compareTo(o.defined);
		}
		return super.compareTo(other);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof DefineSymbol) {
			return this.name.equals(((DefineSymbol) other).name) && this.defined.equals(((DefineSymbol) other).defined);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.defined.hashCode() * this.name.hashCode();
	}
}
