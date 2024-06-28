package velka.core.application;

import java.util.Arrays;
import java.util.Optional;

import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.TypeHolder;
import velka.core.interpretation.Environment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.types.TypesDoesNotUnifyException;
import velka.util.AppendableException;
import velka.util.NameGenerator;
import velka.util.Pair;

/**
 * Expression for interpretation and handling the define special form. Purely
 * for purposes of simplifying the code.
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class DefineSymbol extends Expression {
	
	/**
	 * Symbol for define special form
	 */
	public static final String DEFINE = "define";

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
		//Symbol already exists in this environment, this is a redefinition
		if(env.containsVariable(this.name)) {
			Pair<Type, Substitution> infered = this.defined.infer(env);
			return infered;
		}
		//Symbol does not exists in environment, create mockup to allow recursion
		Environment childEnv = Environment.create(env);
		TypeVariable tv = new TypeVariable(NameGenerator.next());
		childEnv.put(this.name, new TypeHolder(tv));
		
		Pair<Type, Substitution> infered = this.defined.infer(childEnv);

		Substitution s = infered.second;
		Substitution tmp;

		if (!s.containsVariable(tv)) {
			tmp = new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(tv, infered.first)));
		} else {
			Optional<Substitution> opt = Type.unifyTypes(s.get(tv).get(), infered.first);
			if(opt.isEmpty()) {
				throw new TypesDoesNotUnifyException(s.get(tv).get(), infered.first);
			}
			
			tmp = opt.get();
		}

		return new Pair<Type, Substitution>(infered.first, s.compose(tmp));
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		Environment e = Environment.create(env);
		Pair<Type, Substitution> innerInfered = this.inferDefined(env);
		e.put(this.name, new TypeHolder(innerInfered.first, this.name));
		Expression interpreted = this.defined.interpret(e);
		//if(!env.containsVariable(this.name)) {
			env.put(this.name, interpreted);
		//}
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
		s.append(this.name.name);
		s.append(" ");
		Environment inferenceEnvironment = Environment.create(env);
		inferenceEnvironment.put(this.name, new TypeHolder(new TypeVariable(NameGenerator.next())));
		Type t = this.defined.infer(inferenceEnvironment).first;
		if(!env.containsVariable(this.name)) {
			env.put(this.name, new TypeHolder(t));
		}
		s.append(this.defined.toClojureCode(env));
		s.append(")");
		return s.toString();
	}

	@Override
	public String toString() {
		return "(" + DEFINE + " " + this.name.toString() + " " + this.defined.toString() + ")";
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

	@Override
	protected Expression doConvert(Type from, Type to, Environment env)
			throws AppendableException {
		Expression e = this.interpret(env);
		return e.convert(to, env);
	}
}
