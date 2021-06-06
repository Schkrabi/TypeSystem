package velka.lang.abstraction;

import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
import velka.lang.expression.TypeHolder;
import velka.lang.expression.Symbol;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeArrow;
import velka.lang.types.TypeTuple;
import velka.lang.types.TypeVariable;
import velka.lang.util.AppendableException;
import velka.lang.util.NameGenerator;
import velka.lang.util.Pair;
import velka.lang.interpretation.ClojureCodeGenerator;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;

/**
 * Simple lambda expression
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Lambda extends Abstraction implements Comparable<Expression> {

	/**
	 * Formal arguments (names) of the lambda expression
	 */
	public final Tuple args;

	/**
	 * Body
	 */
	public final Expression body;

	/**
	 * Non mandatory type of the lambda arguments
	 */
	public final TypeTuple argsType;

	/**
	 * General identity lambda
	 */
	public static final Lambda identity = Lambda.makeIdentity(new TypeVariable(NameGenerator.next()));

	public Lambda(Tuple args, TypeTuple argsType, Expression body) {
		this.args = args;
		this.body = body;
		this.argsType = argsType;
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) {
		return new Function(this.argsType, this.args, this.body, env);
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("(lambda (");

		Iterator<Expression> i = this.args.iterator();
		Iterator<Type> j = this.argsType.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			Type t = j.next();

			if (t instanceof TypeVariable) {
				s.append(e.toString());
			} else {
				s.append('(');
				s.append(t.toString());
				s.append(' ');
				s.append(e.toString());
				s.append(')');
			}
		}

		s.append(") ");
		s.append(this.body.toString());
		s.append(')');

		return s.toString();
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		try {
			// First infer types in body, use typeholders for argument variables
			Environment childEnv = Environment.create(env);
			List<Type> argsTypeArr = new LinkedList<Type>();

			Iterator<Type> i = this.argsType.iterator();
			for (Expression e : this.args) {
				if (!(e instanceof Symbol)) {
					// TODO change throwable
					throw new AppendableException(e + " is not instance of " + Symbol.class.getName());
				}
				Type t = i.next();
				childEnv.put((Symbol) e, new TypeHolder(t));
				argsTypeArr.add(t);
			}

			Type argsType = new TypeTuple(argsTypeArr);

			Pair<Type, Substitution> bodyInfered = this.body.infer(childEnv, typeEnv);

			// Update argument type with found bindings
			argsType = argsType.apply(bodyInfered.second);

			return new Pair<Type, Substitution>(new TypeArrow(argsType, bodyInfered.first.apply(bodyInfered.second)),
					Substitution.EMPTY);

		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	/**
	 * Creates code of this lambda as simple lambda in Clojure (e.g. (fn [x] x))
	 * 
	 * @param expectedType expected type of this lambda
	 * @param env          environment where lambda is evaluated
	 * @param typeEnv 
	 * @return string containing clojure code
	 * @throws AppendableException Thrown on unification error or when any argument
	 *                             is not a variable
	 */
	protected String toClojureFn(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		StringBuilder s = new StringBuilder();
		
		s.append("(fn [");

		Iterator<Expression> i = this.args.iterator();
		Iterator<Type> j = this.argsType.iterator();
		Environment child = Environment.create(env);
		while (i.hasNext()) {
			Expression e = i.next();
			Type t = j.next();
			if (!(e instanceof Symbol)) {
				// TODO change throwable
				throw new AppendableException("Invalid expression in lambda variable list!");
			}
			Symbol v = (Symbol) e;
			child.put(v, new TypeHolder(t));
			s.append(v.toClojureCode(env, typeEnv));
			if (i.hasNext()) {
				s.append(' ');
			}
		}
		s.append("] \n");

		s.append(this.body.toClojureCode(child, typeEnv));
		s.append(")");
		
		Pair<Type, Substitution> p = this.infer(env, typeEnv);
		
		return ClojureCodeGenerator.addTypeMetaInfo(s.toString(), p.first);
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof Lambda) {
			int cmp = this.args.compareTo(((Lambda) other).args);
			if (cmp != 0) {
				return cmp;
			}

			cmp = this.argsType.compareTo(((Lambda) other).argsType);
			if (cmp != 0) {
				return cmp;
			}

			return this.body.compareTo(((Lambda) other).body);
		}
		return super.compareTo(other);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Lambda) {
			boolean argsEqual = this.args.equals(((Lambda) other).args);
			if(!argsEqual) {
				return false;
			}
			boolean bodyEqual = this.body.equals(((Lambda) other).body);
			if(!bodyEqual) {
				return false;
			}
			
			return Type.unifyRepresentation(this.argsType, ((Lambda) other).argsType).isPresent();
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.args.hashCode() * this.body.hashCode();
	}

	@Override
	protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
			Optional<Expression> rankingFunction) throws AppendableException {
		Function f = (Function) this.interpret(env, typeEnv);
		return f.doSubstituteAndEvaluate(args, env, typeEnv, rankingFunction);
	}

	/**
	 * Makes identity lambda with given type
	 * 
	 * @param argType type of the identity arg
	 * @return identity lambda
	 */
	public static Lambda makeIdentity(Type argType) {
		Symbol symbol = new Symbol(NameGenerator.next());
		return new Lambda(new Tuple(Arrays.asList(symbol)), new TypeTuple(Arrays.asList(argType)), symbol);
	}

	@Override
	protected String implementationsToClojure(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		StringBuilder sb = new StringBuilder();
		
		sb.append("(let [impl ");
		sb.append(this.toClojureFn(env, typeEnv));
		sb.append("] \n");
		sb.append("(fn ");
		sb.append("([args] impl) \n");
		sb.append("([args ranking-fn] impl)))");
		
		return sb.toString();
	}

	@Override
	public Abstraction selectImplementation(Tuple args, Optional<Expression> rankingFunction, Environment env,
			TypeEnvironment typeEnv) throws AppendableException {
		return this;
	}
}
